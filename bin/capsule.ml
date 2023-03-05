module M = Mehari_lwt_unix
open Lwt.Infix
open Lwt.Syntax

let program ~target =
  let open Yocaml.Syntax in
  let* () = Task.move_images target in
  let* () = Task.move_index target in
  let* () = Task.process_articles target in
  let* () = Task.process_pages target in
  let* () = Task.generate_gemlog target in
  let* () = Task.generate_feed target in
  let* () = Task.generate_tags target in
  Task.generate_tags_index target

(* Taken from https://github.com/dinosaure/blogger/blob/main/src/blogger.ml *)
module SSH = struct
  open Lwt.Infix

  type error = Unix.error * string * string
  type write_error = [ `Closed | `Error of Unix.error * string * string ]

  let pp_error ppf (err, f, v) =
    Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)

  let pp_write_error ppf = function
    | `Closed -> Fmt.pf ppf "Connection closed by peer"
    | `Error (err, f, v) -> Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)

  type flow = { ic : in_channel; oc : out_channel }

  type endpoint = {
    user : string;
    path : string;
    host : Unix.inet_addr;
    port : int;
    capabilities : [ `Rd | `Wr ];
  }

  let pp_inet_addr ppf inet_addr =
    Fmt.string ppf (Unix.string_of_inet_addr inet_addr)

  let connect { user; path; host; port; capabilities } =
    let edn = Fmt.str "%s@%a" user pp_inet_addr host in
    let cmd =
      match capabilities with
      | `Wr -> Fmt.str {sh|git-receive-pack '%s'|sh} path
      | `Rd -> Fmt.str {sh|git-upload-pack '%s'|sh} path
    in
    let cmd = Fmt.str "ssh -p %d %s %a" port edn Fmt.(quote string) cmd in
    try
      let ic, oc = Unix.open_process cmd in
      Lwt.return_ok { ic; oc }
    with Unix.Unix_error (err, f, v) -> Lwt.return_error (`Error (err, f, v))

  let read t =
    let tmp = Bytes.create 0x1000 in
    try
      let len = input t.ic tmp 0 0x1000 in
      if len = 0 then Lwt.return_ok `Eof
      else Lwt.return_ok (`Data (Cstruct.of_bytes tmp ~off:0 ~len))
    with Unix.Unix_error (err, f, v) -> Lwt.return_error (err, f, v)

  let write t cs =
    let str = Cstruct.to_string cs in
    try
      output_string t.oc str;
      flush t.oc;
      Lwt.return_ok ()
    with Unix.Unix_error (err, f, v) -> Lwt.return_error (`Error (err, f, v))

  let writev t css =
    let rec go t = function
      | [] -> Lwt.return_ok ()
      | x :: r -> (
          write t x >>= function
          | Ok () -> go t r
          | Error _ as err -> Lwt.return err)
    in
    go t css

  let close t =
    close_in t.ic;
    close_out t.oc;
    Lwt.return_unit
end

let ssh_edn, ssh_protocol = Mimic.register ~name:"ssh" (module SSH)

let unix_ctx_with_ssh () =
  let open Lwt.Infix in
  Git_unix.ctx (Happy_eyeballs_lwt.create ()) >|= fun ctx ->
  let open Mimic in
  let k0 scheme user path host port capabilities =
    match (scheme, Unix.gethostbyname host) with
    | `SSH, { Unix.h_addr_list; _ } when Array.length h_addr_list > 0 ->
        Lwt.return_some
          { SSH.user; path; host = h_addr_list.(0); port; capabilities }
    | _ -> Lwt.return_none
  in
  ctx
  |> Mimic.fold Smart_git.git_transmission
       Fun.[ req Smart_git.git_scheme ]
       ~k:(function `SSH -> Lwt.return_some `Exec | _ -> Lwt.return_none)
  |> Mimic.fold ssh_edn
       Fun.
         [
           req Smart_git.git_scheme;
           req Smart_git.git_ssh_user;
           req Smart_git.git_path;
           req Smart_git.git_hostname;
           dft Smart_git.git_port 22;
           req Smart_git.git_capabilities;
         ]
       ~k:k0

let build_and_push remote author email hook =
  let run () =
    let fail msg = failwith ("build-and-push: " ^ msg) in
    let* ctx = unix_ctx_with_ssh () in
    Yocaml_git.execute
      (module Yocaml_unix)
      (module Pclock)
      ~author ~email ~ctx remote (program ~target:"")
    >>= function
    | Ok () -> (
        match hook with
        | None -> Lwt.return_unit
        | Some hook -> (
            match Razzia.make_request (Uri.of_string hook) with
            | Ok req -> (
                Razzia_unix.get req >>= function
                | Ok (Success { body; _ }) -> Lwt_io.printl body
                | Ok resp -> Format.kasprintf fail "%a" Razzia.pp_response resp
                | Error err -> Format.kasprintf fail "%a" Razzia.pp_err err)
            | Error err -> Format.kasprintf fail "%a" Razzia.pp_request_err err)
        )
    | Error (`Msg msg) -> Format.kasprintf fail "%s." msg
  in
  Lwt_main.run (run ())

let watch dir =
  let run () =
    let* certchains =
      X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
      >|= fun cert -> [ cert ]
    in
    M.router [ M.route ~regex:true "/(.*)" (M.static dir) ]
    |> Mehari_lwt_unix.logger
    |> Mehari_lwt_unix.run_lwt ~certchains
  in
  Yocaml_unix.execute (program ~target:dir);
  Lwt_main.run (run ())

let watch_cmd =
  let open Cmdliner in
  let doc =
    Format.asprintf "Serve from the specified directory as a Gemini server."
  in
  let default_dir = "_site" in
  let dir_arg =
    let doc =
      Format.asprintf "Specify where we build the website (default: %S)"
        default_dir
    in
    let arg = Arg.info ~doc [ "destination" ] in
    Arg.(value & opt string default_dir & arg)
  in
  let info = Cmd.info "watch" ~doc in
  Cmd.v info Term.(const watch $ dir_arg)

let push_cmd =
  let open Cmdliner in
  let doc = Format.asprintf "Push the blog into a Git repository" in
  let remote_arg =
    let remote =
      let parser str =
        match Smart_git.Endpoint.of_string str with
        | Ok _ -> Ok str
        | Error _ as err -> err
      in
      Arg.conv (parser, Fmt.string)
    in
    let doc = "The remote Git repository" in
    let arg = Arg.info ~doc [ "r"; "remote" ] in
    Arg.(required & opt (some remote) None & arg)
  in
  let hook_arg =
    let doc = "The URL of the hook to update the unikernel" in
    let arg = Arg.info ~doc [ "h"; "hook" ] in
    Arg.(value & opt (some string) None & arg)
  in
  let name_arg =
    let doc = "Name of the committer." in
    Cmdliner.Arg.(value & opt string "" & info [ "name" ] ~doc)
  in
  let email_arg =
    let doc = "Email of the committer." in
    Cmdliner.Arg.(value & opt string "" & info [ "email" ] ~doc)
  in
  let info = Cmd.info "push" ~doc in
  Cmd.v info
    Term.(const build_and_push $ remote_arg $ name_arg $ email_arg $ hook_arg)

let cmd =
  let open Cmdliner in
  let default_info = Cmd.info Sys.argv.(0) in
  Cmd.group default_info [ watch_cmd; push_cmd ]

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  exit (Cmdliner.Cmd.eval cmd)

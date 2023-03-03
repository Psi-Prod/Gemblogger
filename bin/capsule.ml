module M = Mehari_lwt_unix
open Lwt.Syntax

let program =
  let open Yocaml.Syntax in
  let* () = Task.move_images in
  let* () = Task.move_index in
  let* () = Task.process_articles in
  let* () = Task.process_pages in
  let* () = Task.generate_gemlog in
  let* () = Task.generate_feed in
  let* () = Task.generate_tags in
  Task.generate_tags_index

let main () =
  let* certchains = Config.certs in
  M.router [ M.route ~regex:true "/(.*)" (M.static "_site") ]
  |> Mehari_lwt_unix.logger
  |> Mehari_lwt_unix.run_lwt ~v4:Config.addr ~certchains

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  Yocaml_unix.execute program;
  Lwt_main.run (main ())

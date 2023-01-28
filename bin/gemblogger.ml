let program =
  let open Yocaml in
  let* () = Task.process_articles in
  let* () = Task.process_pages in
  let* () = Task.generate_gemlog in
  let* () = Task.generate_feed in
  (* let* () = Task.generate_tags in *)
  Task.generate_tags

let build () = Yocaml_unix.execute program

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  build ();
  Mehari_lwt_unix.router
    [
      Mehari_lwt_unix.route ~typ:`Regex "/(.*)"
        (Mehari_lwt_unix.static "_build/");
    ]
  |> Mehari_lwt_unix.run

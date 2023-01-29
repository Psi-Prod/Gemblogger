let program =
  let open Yocaml in
  let* () = Task.move_images in
  let* () = Task.move_index in
  let* () = Task.process_articles in
  let* () = Task.process_pages in
  let* () = Task.generate_gemlog in
  let* () = Task.generate_feed in
  let* () = Task.generate_tags in
  Task.generate_tags_index

let build () = Yocaml_unix.execute program

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  build ();
  Mehari_lwt_unix.router
    [
      Mehari_lwt_unix.route ~typ:`Regex "/(.*)"
        (Mehari_lwt_unix.static "_site/");
    ]
  |> Mehari_lwt_unix.run

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

let router cwd =
  Mehari_eio.router
    [
      Mehari_eio.route ~typ:`Regex "/(.*)"
        (Mehari_eio.static Eio.Path.(cwd / "_site"));
    ]

let main ~net ~cwd =
  Mehari_eio.run net
    ~certchains:Eio.Path.[ (cwd / "cert.pem", cwd / "key.pem") ]
    (router cwd)

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  build ();
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~net:env#net ~cwd:env#cwd

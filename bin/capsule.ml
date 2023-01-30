let build_dir = "_site"

let article_or_not_found ~cwd article f =
  let article_url = Yocaml.add_extension article "gmi" in
  let dir = Eio.Path.(cwd / build_dir / "articles") in
  let articles = Eio.Path.read_dir dir in
  if List.mem article_url articles then f (dir, article_url)
  else Mehari.(response not_found) ""

let post_comment store cwd req =
  article_or_not_found ~cwd (Mehari.param req 1) (fun (_, article_url) ->
      match Mehari.query req with
      | None -> Mehari.(response input) "Votre commentaire"
      | Some content ->
          let author =
            (match Mehari.client_cert req with
            | [] -> None
            | hd :: _ ->
                X509.Certificate.issuer hd
                |> X509.Distinguished_name.common_name)
            |> Option.value ~default:"Anonyme"
          in
          Lwt_eio.run_lwt (fun () ->
              Database.post_comment store article_url ~author
                ~content:(Uri.pct_decode content));
          Filename.concat "/articles" article_url
          |> Mehari.(response redirect_temp))

let commentaries_template ~article_url ~comment_nb =
  let open Mehari.Gemtext in
  [
    newline;
    heading `H1 (Printf.sprintf "Commentaires (%i)" comment_nb);
    newline;
    link
      (Printf.sprintf "/articles/%s/comment" article_url)
      ~name:"Écrire un commentaire";
    newline;
    newline;
  ]

let no_commentay ~article_url =
  let open Mehari.Gemtext in
  [
    newline;
    heading `H1 "Commentaires";
    newline;
    text "Pas encore de commentaires";
    newline;
    link
      (Printf.sprintf "/articles/%s/comment" article_url)
      ~name:"Écrire un commentaire";
    newline;
  ]

let commentary_template (c : Comment.t) =
  let open Mehari.Gemtext in
  [
    text (Format.asprintf "Par %s, le %a :" c.author Comment.pp_date_hum c.date);
    quote c.content;
    newline;
    newline;
  ]

let serve_article store cwd req =
  article_or_not_found ~cwd (Mehari.param req 1) (fun (dir, article_url) ->
      let content = Eio.Path.(load (dir / article_url)) in
      let body =
        match
          Lwt_eio.run_lwt (fun () -> Database.get_comments store article_url)
        with
        | None ->
            Mehari.stream (fun consume ->
                consume content;
                no_commentay ~article_url |> Mehari.Gemtext.to_string |> consume)
        | Some (comments, comment_nb) ->
            Mehari.stream (fun consume ->
                consume content;
                commentaries_template ~article_url ~comment_nb
                |> Mehari.Gemtext.to_string |> consume;
                List.iter
                  (fun c ->
                    commentary_template c |> Mehari.Gemtext.to_string |> consume)
                  comments)
      in
      let mime = Mehari.gemini ~charset:"utf-8" ~lang:[ "fr" ] () in
      Mehari.response_body body mime)

let serve_misc _ =
  let open Mehari in
  let fortune =
    let ic = Unix.open_process_in "forchan" in
    let line = input_line ic in
    Unix.close_process_in ic |> ignore;
    line
  in
  let year, month, day = Ptime_clock.now () |> Ptime.to_date in
  let body =
    Gemtext.
      [
        heading `H1 "Misc";
        newline;
        heading `H2 "Check your fortune (powered by forchan):";
        newline;
        quote fortune;
        newline;
        heading `H2 (Format.asprintf "Today: %i/%i/%i" year month day);
        newline;
        link "/" ~name:"Back to home";
      ]
  in
  response_body (gemtext body) (gemini ~charset:"utf-8" ~lang:[ "en" ] ())

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

let router store cwd =
  let regex_route = Mehari_eio.route ~typ:`Regex in
  Mehari_eio.router
    [
      Mehari_eio.route "/misc.gmi" serve_misc;
      regex_route {|/articles/([a-zA-Z0-9_-]+).gmi/comment|}
        (post_comment store cwd);
      regex_route {|/articles/([a-zA-Z0-9_-]+).gmi|} (serve_article store cwd);
      regex_route "/(.*)" (Mehari_eio.static Eio.Path.(cwd / build_dir));
    ]

let main ~net ~cwd =
  let config = Irmin_git.config "_store" in
  let repo = Lwt_eio.run_lwt (fun () -> Database.Store.Repo.v config) in
  let store = Lwt_eio.run_lwt (fun () -> Database.Store.main repo) in
  router store cwd |> Mehari_eio.logger
  |> Mehari_eio.run net
       ~certchains:Eio.Path.[ (cwd / "cert.pem", cwd / "key.pem") ]

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  Eio_main.run (fun env ->
      build ();
      Lwt_eio.with_event_loop ~clock:env#clock (fun _ ->
          main ~net:env#net ~cwd:env#cwd))

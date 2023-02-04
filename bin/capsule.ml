open Lwt.Syntax

let build_dir = "_site"
let articles_dir = "_site/articles"
let ( / ) = Filename.concat

let article_or_not_found article f =
  let article_url = Yocaml.add_extension article "gmi" in
  let* articles =
    Lwt_unix.files_of_directory articles_dir |> Lwt_stream.to_list
  in
  if List.mem article_url articles then f (articles_dir, article_url)
  else Mehari_lwt_unix.respond Mehari.not_found ""

let post_comment store req =
  article_or_not_found (Mehari.param req 1) (fun (_, article_url) ->
      match Mehari.query req with
      | None -> Mehari_lwt_unix.respond Mehari.input "Votre commentaire"
      | Some content ->
          let author =
            (match Mehari.client_cert req with
            | [] -> None
            | hd :: _ ->
                X509.Certificate.issuer hd
                |> X509.Distinguished_name.common_name)
            |> Option.value ~default:"Anonyme"
          in
          let+ () =
            Database.post_comment store article_url ~author
              ~content:(Uri.pct_decode content)
          in
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
    heading `H1 "Commentaires (0)";
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
  ]

let serve_article store req =
  article_or_not_found (Mehari.param req 1) (fun (dir, article_url) ->
      let* content =
        Lwt_io.with_file (dir / article_url) ~mode:Input Lwt_io.read
      in
      let+ comments = Database.get_comments store article_url in
      let body =
        match comments with
        | None ->
            content ^ (no_commentay ~article_url |> Mehari.Gemtext.to_string)
        | Some (comments, comment_nb) ->
            let header =
              commentaries_template ~article_url ~comment_nb
              |> Mehari.Gemtext.to_string
            in
            let comments =
              List.map
                (fun c -> commentary_template c |> Mehari.Gemtext.to_string)
                comments
              |> String.concat "\n"
            in
            content ^ header ^ comments
      in
      let mime = Mehari.gemini ~charset:"utf-8" ~lang:[ "fr" ] () in
      Mehari.(response_body (string body) mime))

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
  Mehari_lwt_unix.respond_body (gemtext body)
    (gemini ~charset:"utf-8" ~lang:[ "en" ] ())

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

let router store =
  let regex_route = Mehari_lwt_unix.route ~typ:`Regex in
  Mehari_lwt_unix.router
    [
      Mehari_lwt_unix.route "/misc.gmi" serve_misc;
      regex_route {|/articles/([a-zA-Z0-9_-]+).gmi/comment|}
        (post_comment store);
      regex_route {|/articles/([a-zA-Z0-9_-]+).gmi|} (serve_article store);
      regex_route "/(.*)" (Mehari_lwt_unix.static build_dir);
    ]

let main () =
  let config = Irmin_git.config "_store" in
  let* repo = Database.Store.Repo.v config in
  let* store = Database.Store.main repo in
  router store |> Mehari_lwt_unix.logger
  |> Mehari_lwt_unix.run_lwt ~certchains:Config.certs

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  build ();
  Lwt_main.run (main ())

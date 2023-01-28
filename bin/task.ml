open Yocaml
module Metaformat = Yocaml_yaml
module Template = Yocaml_jingoo

let target = "_build/"
let template file = add_extension file "gmi" |> into "templates"
let article_template = template "article"
let list_template = template "list_articles"
let article_target file = Model.article_path file |> into target
let binary_update = Build.watch Sys.argv.(0)
let index_gmi = "index.gmi" |> into "pages"
let index = "index.gmi" |> into target
let rss_feed = "feed.xml" |> into target
let tag_file tag = Model.tag_path tag |> into target
let tag_template = template "tag"

let process_articles =
  process_files [ "articles" ] File.is_gemtext (fun article_file ->
      let open Build in
      create_file
        (article_target article_file)
        (binary_update
        >>> Metaformat.read_file_with_metadata
              (module Model.Article)
              article_file
        >>> Template.apply_as_template (module Model.Article) article_template
        >>^ Stdlib.snd))

let merge_with_page ((meta, content), articles) =
  let title = Metadata.Page.title meta in
  let description = Metadata.Page.description meta in
  (Model.Articles.make ?title ?description articles, content)

let generate_feed =
  let open Build in
  let* articles_arrow =
    Collection.Articles.get_all (module Metaformat) "articles"
  in
  create_file rss_feed
    (binary_update >>> articles_arrow >>^ Feed.make >>^ Rss.Channel.to_rss)

let generate_tags =
  let* deps, tags = Collection.Tags.compute (module Metaformat) "articles" in
  let tags_string = List.map (fun (i, s) -> (i, List.length s)) tags in
  let mk_meta tag articles () = (Model.Tag.make tag articles tags_string, "") in
  List.fold_left
    (fun program (tag, articles) ->
      let open Build in
      program
      >> create_file (tag_file tag)
           (init deps >>> binary_update >>^ mk_meta tag articles
           >>> Template.apply_as_template (module Model.Tag) tag_template
           >>^ Stdlib.snd))
    (return ()) tags

let generate_index =
  let open Build in
  let* articles_arrow =
    Collection.Articles.get_all (module Metaformat) "articles"
  in
  create_file index
    (binary_update
    >>> Metaformat.read_file_with_metadata (module Metadata.Page) index_gmi
    >>> articles_arrow >>^ merge_with_page
    >>> Template.apply_as_template (module Model.Articles) list_template
    >>^ Stdlib.snd)

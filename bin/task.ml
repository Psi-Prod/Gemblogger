open Yocaml
module Metaformat = Yocaml_yaml
module Template = Yocaml_jingoo

let target = "_site/"
let template file = add_extension file "gmi" |> into "templates"
let article_template = template "article"
let layout_template = template "layout"
let gemlog_template = template "gemlog"
let tags_index_template = template "tags"
let tags_index = "browse-tag.gmi" |> into target
let article_target file = Model.article_path file |> into target
let binary_update = Build.watch Sys.argv.(0)
let gemlog = "gemlog.gmi" |> into target
let rss_feed = "feed.xml" |> into target
let tag_file tag = Model.tag_path tag |> into target
let tag_template = template "tag"

let move_index =
  process_files [ "pages" ] File.is_index (Build.copy_file ~into:target)

let process_pages =
  process_files [ "pages" ]
    Preface.Predicate.(File.is_gemtext && !File.is_index)
    (fun file ->
      let target = basename file |> into target in
      let open Build in
      create_file target
        (binary_update
        >>> Yocaml_yaml.read_file_with_metadata (module Metadata.Page) file
        >>> Template.apply_as_template (module Metadata.Page) layout_template
        >>^ Stdlib.snd))

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
        >>> Template.apply_as_template (module Model.Article) layout_template
        >>^ Stdlib.snd))

let generate_gemlog =
  let open Build in
  let* articles_arrow =
    Collection.Articles.get_all (module Metaformat) "articles"
  in
  create_file gemlog
    (binary_update >>> articles_arrow
    >>^ (fun ((), articles) -> (Model.Articles.make articles, ""))
    >>> Template.apply_as_template (module Model.Articles) gemlog_template
    >>> Template.apply_as_template (module Model.Articles) layout_template
    >>^ Stdlib.snd)

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
           >>> Template.apply_as_template (module Model.Tag) layout_template
           >>^ Stdlib.snd))
    (return ()) tags

let generate_tags_index =
  let open Build in
  let* deps, tags = Collection.Tags.compute (module Metaformat) "articles" in
  create_file tags_index
    (init deps >>> binary_update
    >>^ (fun () -> (Model.Tags.make tags, ""))
    >>> Template.apply_as_template (module Model.Tags) tags_index_template
    >>^ Stdlib.snd)

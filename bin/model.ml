open Yocaml

let article_path file =
  let filename = basename $ replace_extension file "gmi" in
  filename |> into "articles"

let tag_path tag = add_extension tag "gmi" |> into "tags"

module Author = struct
  type t = string

  let from (type a) (module V : Metadata.VALIDABLE with type t = a) obj =
    V.string obj

  let inject (type a) (module D : Key_value.DESCRIBABLE with type t = a) name =
    D.[ ("name", string name) ]
end

module Article = struct
  type t = {
    title : string;
    date : Date.t;
    tags : string list;
    description : string option;
    authors : Author.t list;
  }

  let date { date; _ } = date
  let tags { tags; _ } = tags

  let to_rss_item url article =
    let open Rss in
    Item.make ~title:article.title ~link:url ~pub_date:article.date
      ~description:(Option.value ~default:"" article.description)
      ~guid:(Guid.link url) ()
      ~categories:
        (List.map (fun c -> Category.make ~category:c ()) article.tags)

  let make title date description authors tags =
    {
      title;
      date;
      description;
      authors;
      tags = List.map String.lowercase_ascii tags;
    }

  let from_string (module V : Metadata.VALIDABLE) = function
    | None -> Validate.error $ Error.Required_metadata [ "Article" ]
    | Some str ->
        let open Validate.Monad in
        V.from_string str
        >>= V.object_and (fun assoc ->
                let open Validate.Applicative in
                make
                <$> V.(required_assoc string) "title" assoc
                <*> V.required_assoc
                      (Metadata.Date.from (module V))
                      "date" assoc
                <*> V.(optional_assoc string) "description" assoc
                <*> V.(required_assoc (list_of (Author.from (module V))))
                      "authors" assoc
                <*> V.(optional_assoc_or ~default:[] (list_of string))
                      "tags" assoc)

  let inject (type a) (module D : Key_value.DESCRIBABLE with type t = a)
      { title; date; description; authors; tags } =
    let description =
      Option.fold description ~none:[] ~some:(fun d ->
          [ ("description", D.string d) ])
    in
    D.
      [
        ("title", string title);
        ("tags", list (List.map string tags));
        ("date", object_ $ Metadata.Date.inject (module D) date);
        ( "authors",
          list
            (List.map (fun a -> object_ $ Author.inject (module D) a) authors)
        );
      ]
    @ description

  let compare_by_date a b = Date.compare a.date b.date
end

module Articles = struct
  type t = (Article.t * string) list

  let make a = a
  let articles a = a

  let sort ?(decreasing = true) articles =
    List.sort
      (fun (a, _) (b, _) ->
        let a_date = Article.date a and b_date = Article.date b in
        let r = Date.compare a_date b_date in
        if decreasing then ~-r else r)
      articles

  let sort_articles_by_date ?(decreasing = true) articles =
    sort ~decreasing articles

  let inject (type a) (module D : Key_value.DESCRIBABLE with type t = a)
      articles =
    ( "articles",
      D.list
        (List.map
           (fun (article, url) ->
             D.object_
               (("url", D.string url) :: Article.inject (module D) article))
           articles) )
    :: (Metadata.Page.inject (module D) $ Metadata.Page.make None None)
end

let article_object (type a) (module D : Key_value.DESCRIBABLE with type t = a)
    (article, url) =
  D.object_ (("url", D.string url) :: Article.inject (module D) article)

module Tag = struct
  type t = {
    tag : string;
    tags : (string * int) list;
    articles : (Article.t * string) list;
  }

  let make tag articles tags = { tag; tags; articles = Articles.sort articles }

  let inject (type a) (module D : Key_value.DESCRIBABLE with type t = a)
      { tag; tags; articles } =
    ("tag", D.string tag)
    :: ("articles", D.list (List.map (article_object (module D)) articles))
    :: ( "tags",
         D.list
           (List.map
              (fun (tag, n) ->
                D.object_
                  [
                    ("name", D.string tag);
                    ("link", D.string (tag_path tag));
                    ("number", D.integer n);
                  ])
              tags) )
    :: (Metadata.Page.inject (module D) $ Metadata.Page.make None None)
end

module Tags = struct
  type t = (string * int) list

  let make articles =
    List.map (fun (tag, articles) -> (tag, List.length articles)) articles
    |> List.sort (fun (t, _) (t', _) -> String.compare t t')

  let inject (type a) (module D : Key_value.DESCRIBABLE with type t = a) tags =
    ( "tags",
      D.list
        (List.map
           (fun (tag, n) ->
             D.object_
               [
                 ("name", D.string tag);
                 ("link", D.string (tag_path tag));
                 ("number", D.integer n);
               ])
           tags) )
    :: (Metadata.Page.inject (module D) $ Metadata.Page.make None None)
end

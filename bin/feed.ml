open Yocaml

let domain = "heyplzlookat.me"
let feed_url = into domain "atom.xml"
let icon_url = into domain "images/icon.png" |> Uri.of_string

let articles_to_items articles =
  List.map
    (fun (article, url) ->
      Model.Article.to_atom_entry (into domain url) article)
    articles

let tim =
  Syndic.Atom.author
    ~uri:(Uri.of_string "tim-ats-d.srht.site/")
    ~email:"tim.arnouts@protonmail" "Tim"

let leo = Syndic.Atom.author ~email:"lelolartichaut@laposte.net" "Léo"

let make ((), articles) =
  let updated =
    match
      Model.Articles.make articles
      |> Model.Articles.sort_articles_by_date |> Model.Articles.articles
    with
    | [] -> Ptime.epoch
    | (a, _) :: _ -> Model.Article.ptime a
  in
  Yocaml_syndication.Atom.make ~title:(Text "Heyplzlookatme's gemlog")
    ~subtitle:
      (Text
         "We post here our projects, some blog posts, devlogs and political \
          takes in French.")
    ~id:(Uri.of_string feed_url) ~authors:[ leo; tim ] ~updated
    ~links:[ Syndic.Atom.link ~hreflang:"en" (Uri.of_string domain) ]
    ~icon:icon_url
    (articles_to_items articles)

let pp ppf feed =
  Format.fprintf ppf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>%s"
    (Syndic.Atom.to_xml feed
    |> Syndic.XML.to_string ~ns_prefix:(function
         | "http://www.w3.org/2005/Atom" -> Some ""
         | _ -> Some "http://www.w3.org/2005/Atom"))

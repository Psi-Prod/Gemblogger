open Yocaml

let domain = "gemini://heyplzlookat.me"
let feed_url = into domain "feed.xml"

let articles_to_items articles =
  List.map
    (fun (article, url) -> Model.Article.to_rss_item (into domain url) article)
    articles

let make ((), articles) =
  Yocaml.Rss.Channel.make ~title:"Heyplzlookatme's gemlog" ~link:domain
    ~feed_link:feed_url
    ~description:
      "We post here our projects, some blog posts, devlogs and political takes \
       in French."
    ~generator:"YOCaml" ~webmaster:"tim.arnouts@protonmail.com"
    (articles_to_items articles)

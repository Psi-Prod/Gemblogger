open Yocaml

let domain = "gemini://domain"
let feed_url = into domain "feed.xml"

let articles_to_items articles =
  List.map
    (fun (article, url) -> Model.Article.to_rss_item (into domain url) article)
    articles

let make ((), articles) =
  Yocaml.Rss.Channel.make ~title:"My gemlog" ~link:domain ~feed_link:feed_url
    ~description:"My home on gemini space." ~generator:"YOCaml"
    ~webmaster:"mail@mailbox.com"
    (articles_to_items articles)

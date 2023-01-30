module Store = Irmin_git_unix.FS.KV (Comment.Serializable)
module Info = Irmin_git_unix.Info (Store.Info)
open Lwt.Syntax

let post_comment store article ~author ~content =
  let com = Comment.make ~author ~content in
  let info = Info.v "Post comment %a" Comment.pp com in
  Store.with_tree_exn store [ article ] ~info ~strategy:`Set (fun tree ->
      let tree = match tree with Some t -> t | None -> Store.Tree.empty () in
      let id = Format.asprintf "%a" Comment.pp_date com.date in
      let* tree = Store.Tree.add tree [ id ] com in
      Lwt.return_some tree)

let get_comments store article =
  let* mem = Store.mem_tree store [ article ] in
  if mem then
    let* tree = Store.get_tree store [ article ] in
    let* comments, n =
      Store.Tree.fold ~order:`Undefined
        ~contents:(fun _ com (coms, n) -> Lwt.return (com :: coms, succ n))
        tree ([], 0)
    in
    Lwt.return_some (List.sort Comment.compare comments, n)
  else Lwt.return_none

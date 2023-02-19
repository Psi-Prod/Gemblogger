open Yocaml

let is_gemtext = with_extension "gmi"

let is_image =
  let open Preface.Predicate in
  with_extension "png" || with_extension "svg" || with_extension "jpg"
  || with_extension "jpeg" || with_extension "gif"

let is_index = String.starts_with ~prefix:"index"

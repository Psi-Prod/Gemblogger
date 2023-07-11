open Yocaml
open Preface.Predicate

let is_gemtext = with_extension "gmi"

let is_image =
  with_extension "png" || with_extension "svg" || with_extension "jpg"
  || with_extension "jpeg" || with_extension "gif"

let is_audio =
  with_extension "wav" || with_extension "mp3" || with_extension "ogg"

let is_video = with_extension "mp4" || with_extension "webm"
let is_index = String.starts_with ~prefix:"index"

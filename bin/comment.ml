type t = { author : string; date : date; content : string }
and date = (int * int * int) * ((int * int * int) * int)

module Serializable = struct
  type nonrec t = t

  let date =
    Irmin.Type.(pair (triple int int int) (pair (triple int int int) int))

  let t =
    let open Irmin.Type in
    record "comment" (fun author date content -> { author; date; content })
    |+ field "author" string (fun t -> t.author)
    |+ field "date" date (fun t -> t.date)
    |+ field "content" string (fun t -> t.content)
    |> sealr

  let merge = Irmin.Merge.(option (idempotent t))
end

let make ~author ~content =
  let date = Ptime_clock.now () |> Ptime.to_date_time in
  { author; date; content }

let compare { date; _ } { date = date'; _ } =
  Ptime.(
    compare (Option.get (of_date_time date)) (Option.get (of_date_time date')))

let pp_date fmt ((year, month, day), ((hour, min, sec), tz)) =
  Format.fprintf fmt "%i-%i-%i-%i-%i-%i-%i" year month day hour min sec tz

let pp_date_hum fmt ((year, month, day), _) =
  Format.fprintf fmt "%i-%i-%i" year month day

let pp fmt { author; date; content } =
  Format.fprintf fmt "{ name = %S; date = %a; content = %S }" author pp_date
    date content

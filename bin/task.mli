val target : Yocaml.Filepath.t

val move_images : unit Yocaml.Effect.t
val move_index : unit Yocaml.Effect.t
val process_pages : unit Yocaml.Effect.t
val process_articles : unit Yocaml.Effect.t
val generate_gemlog : unit Yocaml.Effect.t
val generate_feed : unit Yocaml.Effect.t
val generate_tags : unit Yocaml.Effect.t
val generate_tags_index : unit Yocaml.Effect.t

type t = int [@@deriving show, eq]

val base : t
val next : t -> t
val prev : t -> t
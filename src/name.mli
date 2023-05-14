type name = string [@@deriving show]
type t = name [@@deriving show]

val make : string -> t
val repr : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
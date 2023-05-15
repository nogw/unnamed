type name = string [@@deriving show]
and t = name

val make : string -> t
val repr : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
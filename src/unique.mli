module Id : sig
  type t = int [@@deriving show]

  val curr : t Atomic.t
  val next : unit -> t
end

type t = { id : Id.t; name : Name.t } [@@deriving show]

val make : Name.t -> t
val create : unit -> t
val equal : t -> t -> bool
val compare : t -> t -> int

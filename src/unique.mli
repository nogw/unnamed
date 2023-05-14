module Id : sig
  type t = int

  val curr : t Atomic.t
  val next : unit -> t
end

type 'name unique = { id : Id.t; name : 'name }
type 'name t = 'name unique

val make : 'name -> 'name t
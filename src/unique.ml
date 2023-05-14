module Id = struct
  type t = int [@@deriving show]

  let curr = Atomic.make 0
  let next () = Atomic.fetch_and_add curr 1
end

type t = { id : Id.t; name : Name.t } [@@deriving show]

let make name =
  let id = Id.next () in
  { id; name }

let create () =
  let id = Id.next () in
  let name = Name.make "Type" in
  { id; name }

let equal lhs rhs =
  let { id = lhs; _ } = lhs in
  let { id = rhs; _ } = rhs in
  Int.equal lhs rhs

let compare lhs rhs =
  let { id = lhs; _ } = lhs in
  let { id = rhs; _ } = rhs in
  Int.compare lhs rhs
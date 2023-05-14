module Id = struct
  type t = int

  let curr = Atomic.make 0
  let next () = Atomic.fetch_and_add curr 1
end

type 'name unique = { id : Id.t; name : 'name }
type 'name t = 'name unique

let make name =
  let id = Id.next () in
  { id; name }

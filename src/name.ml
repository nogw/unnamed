type name = string [@@deriving show]
type t = name [@@deriving show]

let make t = t
let repr t = t
let equal = String.equal
let compare = String.compare
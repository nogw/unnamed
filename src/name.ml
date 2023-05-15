type name = string [@@deriving show]
and t = name

let make t = t
let repr t = t
let equal = String.equal
let compare = String.compare

let pp_name fmt var =
  let name = repr var in
  Format.fprintf fmt "%s" name
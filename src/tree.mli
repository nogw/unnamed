open Name

type id = int [@@deriving show]

type 'link var =
  | Link of 'link
  | Bound of id
  | Generic of id
  | Unbound of id * Level.t
[@@deriving show]

type poly =
  | Ptyp_forall of { param : id list; return : poly }
  | Ptyp_arrow of { param : poly; return : poly }
  | Ptyp_apply of { base : poly; argm : poly }
  | Ptyp_const of { name : name }
  | Ptyp_var of poly var ref
[@@deriving show]

type expr =
  | Pexp_lower of { value : name }
  | Pexp_lambda of { param : name; annot : poly option; body : expr }
  | Pexp_apply of { lambda : expr; argm : expr }
  | Pexp_let of { name : name; bind : expr; body : expr }
  | Pexp_annot of { value : expr; annot : poly }
[@@deriving show]
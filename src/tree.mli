type id = int [@@deriving show, eq]

type 'link var =
  | Link of 'link
  | Bound of id
  | Generic of id
  | Unbound of id * Level.t
[@@deriving show, eq]

type poly =
  | Ptyp_forall of { param : id list; return : poly }
  | Ptyp_arrow of { param : poly; return : poly }
  | Ptyp_apply of { base : poly; argm : poly }
  | Ptyp_const of { name : Name.t }
  | Ptyp_var of poly var ref
[@@deriving show, eq]

type expr =
  | Pexp_lower of { value : Name.t }
  | Pexp_lambda of { param : Name.t; annot : poly option; body : expr }
  | Pexp_apply of { lambda : expr; argm : expr }
  | Pexp_let of { name : Name.t; bind : expr; body : expr }
  | Pexp_annot of { value : expr; annot : poly }
[@@deriving show, eq]
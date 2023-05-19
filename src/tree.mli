type name = Name.t [@@deriving show, eq]

type poly =
  | Ptyp_var of { name : name }
  | Ptyp_const of { name : name }
  | Ptyp_tuple of { types : poly list }
  | Ptyp_apply of { base : poly; argm : poly }
  | Ptyp_arrow of { param : poly; return : poly }
  | Ptyp_forall of { param : name list; return : poly }
[@@deriving show { with_path = false }]

type literal =
  | Plit_unit
  | Plit_number of { value : int }
[@@deriving show { with_path = false }]

type patt =
  | Ppat_wild
  | Ppat_var of { name : name }
  | Ppat_annot of { value : patt; annot : poly }
[@@deriving show { with_path = false }]

type expr =
  | Pexp_literal of { value : literal }
  | Pexp_lower of { value : name }
  | Pexp_let of { name : name; bind : expr; body : expr }
  | Pexp_tuple of { values : expr list }
  | Pexp_annot of { value : expr; annot : poly }
  | Pexp_apply of { lambda : expr; argm : expr }
  | Pexp_lambda of { param : patt; body : expr }
[@@deriving show { with_path = false }]

type term = 
  | PTerm_bind of { name : name; annot : poly option; body : expr } 
[@@deriving show { with_path = false }]
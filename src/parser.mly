%{
  open Tree

  let curry_type_apply base args = 
    List.fold_left (fun base argm -> 
      Ptyp_apply { base; argm }
    ) base args

  let curry_expr_apply lambda args = 
    List.fold_left (fun lambda argm -> 
      Pexp_apply { lambda; argm }
    ) lambda args
%} 

%token <string> LOWER
%token <string> UPPER
%token <int> NUMBER
%token FORALL
%token LAMBDA
%token LET
%token FATARROW
%token ARROW
%token EQUAL
%token COMMA
%token SEMICOLON
%token COLON
%token STAR
%token WILDCARD
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PARENS
%token RIGHT_PARENS
%token LEFT_BRACES
%token RIGHT_BRACES
%token EOF

%start <Tree.term option> term_opt
%start <Tree.expr option> expr_opt
%start <Tree.poly option> type_opt
%%

let type_opt :=
  | EOF; { None }
  | expr = type_entry; EOF; { Some expr }

let expr_opt :=
  | EOF; { None }
  | expr = expr; EOF; { Some expr }

let term_opt :=
  | EOF; { None }
  | expr = term; EOF; { Some expr }

let term :=
  | term_bind
  | term_bind_annot

let term_anno :=
  | name = LOWER; COLON; annot = type_entry; { (name, annot) }
let term_bind :=
  | name = LOWER; EQUAL; body = expr; { PTerm_bind { name = Name.make name; annot = None; body } }
let term_bind_annot :=
  | (_, annot) = term_anno; name = LOWER; EQUAL; body = expr; { PTerm_bind { name = Name.make name; annot = Some(annot); body } }

let expr :=
  | expr_let
  | expr_lambda
  | expr_tuple

let expr_let :=
  | LET; name = LOWER; EQUAL; bind = expr; SEMICOLON; body = expr; { Pexp_let { name = Name.make name; bind; body } }
let expr_lambda :=
  | LAMBDA; param = patt; FATARROW; body = expr; { Pexp_lambda { param; body } }

let expr_tuple :=
  | expr = expr_apply; { expr }
  | expr = expr_apply; COMMA; exprs = nonempty_expr_list; { Pexp_tuple { values = expr :: exprs } }

let nonempty_expr_list :=
  | head = expr_apply; { [head] }
  | head = nonempty_expr_list; COMMA; tail = expr_apply; { head @ [tail] }

let expr_apply :=
  | lambda = expr_apply; args = parens(non_empty_list(COMMA, expr_apply)); { curry_expr_apply lambda args }
  | expr_term

let expr_term :=
  | expr_constraint
  | expr_simple

let expr_simple :=
  | expr_variable
  | expr_literal
  | expr_parens

let expr_literal :=
  | value = lit_entry; { Pexp_literal { value } }
let expr_variable :=
  | name = LOWER; { Pexp_lower { value = Name.make name } }
let expr_constraint :=
  | LEFT_PARENS; (value, annot) = annotation(expr_term); RIGHT_PARENS; { Pexp_annot { value; annot } }
let expr_parens :=
  | LEFT_PARENS; expr = expr; RIGHT_PARENS; { expr }

let lit_entry :=
  | lit_unit
  | lit_number

let lit_unit ==
  | LEFT_PARENS; RIGHT_PARENS; { Plit_unit }
let lit_number ==
  | value = NUMBER; { Plit_number { value } }

let patt :=
  | patt_wild
  | patt_var
  | patt_annot
  | patt_parens

let patt_wild :=
  | WILDCARD; { Ppat_wild }
let patt_var :=
  | name = LOWER; { Ppat_var { name = Name.make name } }
let patt_annot :=
  | LEFT_PARENS; (value, annot) = annotation(patt); RIGHT_PARENS; { Ppat_annot { value; annot } }
let patt_parens :=
  | LEFT_PARENS; patt = patt; RIGHT_PARENS; { patt }

let type_entry :=
  | type_entry_rec

let type_entry_rec :=
  | type_forall
  | type_arrow
  | type_prod

let type_prod :=
  | ty = type_apply; { ty }
  | ty = type_apply; STAR; types = nonempty_type_list; { Ptyp_tuple { types = ty :: types } }

let nonempty_type_list :=
  | head = type_apply; { [head] }
  | head = nonempty_type_list; STAR; tail = type_apply; { head @ [tail] }

let type_apply :=
  | base = type_simple; { base }
  | base = type_apply; args = parens(non_empty_list(COMMA, type_apply)); { curry_type_apply base args }

let type_simple :=
  | type_constructor
  | type_variable
  | type_parens

let type_variable :=
  | value = LOWER; { Ptyp_var { name = Name.make value } }
let type_constructor :=
  | value = UPPER; { Ptyp_const { name = Name.make value } }
let type_arrow :=
  | param = type_simple; ARROW; return = type_entry; { Ptyp_arrow { param; return } }
let type_forall :=
  | FORALL; param = brackets (name_list (COMMA)); FATARROW; return = type_entry; { Ptyp_forall { param; return } }
let type_parens :=
  | LEFT_PARENS; value = type_entry; RIGHT_PARENS; { value } 

let name_list (sep) :=
  | init = LOWER; { [Name.make init] }
  | init = LOWER; sep; rest = name_list (sep); { (Name.make init) :: rest }
let non_empty_list (sep, expr) :=
  | head = expr; { [head] }
  | head = non_empty_list (sep, expr); sep; tail = expr; { head @ [tail] }

let annotation (self) :=
  | value = self; COLON; annot = type_entry; { (value, annot) }
let parens (self) :=
  | LEFT_PARENS; expr = self; RIGHT_PARENS; { expr }
let braces (self) :=
  | LEFT_BRACES; expr = self; RIGHT_BRACES; { expr }
let brackets (self) :=
  | LEFT_BRACKET; expr = self; RIGHT_BRACKET; { expr }
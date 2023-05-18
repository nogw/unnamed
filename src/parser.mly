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
%token FORALL
%token LAMBDA
%token LET
%token FATARROW
%token ARROW
%token EQUAL
%token COMMA
%token SEMICOLON
%token COLON
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
  | expr_apply

let expr_let :=
  | LET; name = LOWER; EQUAL; bind = expr; SEMICOLON; body = expr; { Pexp_let { name = Name.make name; bind; body } }
let expr_lambda :=
  | LAMBDA; (param, annot) = expr_lambda_params; FATARROW; body = expr; { Pexp_lambda { param = Name.make param; annot; body } }

let expr_lambda_params :=
  | param = LOWER; { (param, None) }
  | (param, annot) = parens(annotation(LOWER)); { (param, Some annot) }

let expr_apply :=
  | expr_term
  | lambda = expr_apply; args = parens(non_empty_list(COMMA, expr_apply)); { curry_expr_apply lambda args }

let expr_term :=
  | expr_simple
  | expr_constraint

let expr_simple :=
  | expr_variable
  | expr_parens

let expr_variable :=
  | value = LOWER; { Pexp_lower { value = Name.make value } }
let expr_constraint :=
  | (value, annot) = parens(annotation(expr_term)); { Pexp_annot { value; annot } }
let expr_parens :=
  | LEFT_PARENS; expr = expr; RIGHT_PARENS; { expr }

let type_entry :=
  | type_tuple
  | type_entry_rec

let type_entry_rec :=
  | type_forall
  | type_arrow
  | type_apply

let type_apply :=
  | base = type_apply; args = parens(non_empty_list(COMMA, type_apply)); { curry_type_apply base args }
  | type_simple

let type_simple :=
  | type_constructor
  | type_variable
  | type_parens

(* type variable *)
let type_variable :=
  | value = LOWER; { Ptyp_var { name = Name.make value } }
(* type constructors / builtin *)
let type_constructor :=
  | value = UPPER; { Ptyp_const { name = Name.make value } }
// type tuple
let type_tuple :=
  | types = parens(type_list(COMMA)); { Ptyp_tuple { types } }
(* type arrow *)
let type_arrow :=
  | param = type_simple; ARROW; return = type_entry; { Ptyp_arrow { param; return } }
(* forall type *)
let type_forall :=
  | FORALL; param = brackets (name_list (COMMA)); FATARROW; return = type_entry; { Ptyp_forall { param; return } }
(* type enclosed in parentheses *)
let type_parens :=
  | LEFT_PARENS; value = type_entry; RIGHT_PARENS; { value } 

(* list of names separated by a separator *)
let name_list (sep) :=
  | init = LOWER; { [Name.make init] }
  | init = LOWER; sep; rest = name_list (sep); { (Name.make init) :: rest }
(* list of names separated by a separator *)
let type_list (sep) :=
  | non_empty_list(sep, type_entry)
(* non-empty list of expressions separated by a separator *)
let non_empty_list (sep, expr) :=
  | init = expr; { [init] }
  | init = expr; sep; rest = non_empty_list (sep, expr); { init :: rest }

(* type annotation of a content *)
let annotation (self) :=
  | value = self; COLON; annot = type_entry; { (value, annot) }
(* parentheses around a content *)
let parens (self) :=
  | LEFT_PARENS; expr = self; RIGHT_PARENS; { expr }
(* braces around a content *)
let braces (self) :=
  | LEFT_BRACES; expr = self; RIGHT_BRACES; { expr }
(* brackets around a content *)
let brackets (self) :=
  | LEFT_BRACKET; expr = self; RIGHT_BRACKET; { expr }
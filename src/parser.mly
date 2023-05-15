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

%start <Tree.expr option> term
%start <Tree.poly option> pretype
%%

let pretype :=
  | EOF; { None }
  | expr = type_entry; EOF; { Some expr }

let term :=
  | EOF; { None }
  | expr = expr; EOF; { Some expr }

let expr :=
  | expr_let
  | expr_lambda
  | expr_apply

let expr_let :=
  | LET; name = LOWER; EQUAL; bind = expr; SEMICOLON; body = expr; { Pexp_let { name = Name.make name; bind; body } }
let expr_lambda :=
  | LAMBDA; param = LOWER; FATARROW; body = expr; { Pexp_lambda { param = Name.make param; annot = None; body } }
  | LAMBDA; LEFT_PARENS; param = LOWER; COLON; annot = type_entry; RIGHT_PARENS; FATARROW; body = expr; { Pexp_lambda { param = Name.make param; annot = Some annot; body } }

let expr_apply :=
  | expr_term
  | lambda = expr_apply; args = parens(non_empty_list(COMMA, expr_apply)); { curry_expr_apply lambda args }

let expr_term :=
  | expr_simple
  | expr_annot

let expr_simple :=
  | expr_variable
  | expr_parens

let expr_variable :=
  | value = LOWER; { Pexp_lower { value = Name.make value } }
let expr_parens :=
  | LEFT_PARENS; expr = expr; RIGHT_PARENS; { expr }
let expr_annot :=
  | LEFT_PARENS; value = expr_term; COLON; annot = type_entry; RIGHT_PARENS; { Pexp_annot { value; annot } }

let type_entry :=
  | type_entry_rec

let type_entry_rec :=
  | type_forall
  | type_arrow
  | type_apply

let type_apply :=
  | base = type_apply; args = parens(non_empty_list(COMMA, type_apply)); { curry_type_apply base args }
  | type_simple

let type_simple :=
  | type_variable
  | type_parens

(* type variable *)
let type_variable :=
  | value = LOWER; { Ptyp_const { name = Name.make value } }
(* type arrow *)
let type_arrow :=
  | param = type_simple; ARROW; return = type_entry; { Ptyp_arrow { param; return } }
(* forall type *)
let type_forall :=
  | FORALL; param = braces (name_list (COMMA)); FATARROW; return = type_entry; { Ptyp_forall { param; return } }
(* type enclosed in parentheses *)
let type_parens :=
  | LEFT_PARENS; value = type_entry; RIGHT_PARENS; { value } 

(* non-empty list of expressions separated by a separator *)
let non_empty_list (sep, expr) :=
  | init = expr; { [init] }
  | init = expr; sep; rest = separated_nonempty_list (sep, expr); { init :: rest }
(* list of names separated by a separator *)
let name_list (sep) :=
  | init = LOWER; { [Name.make init] }
  | init = LOWER; sep; rest = name_list (sep); { (Name.make init) :: rest }

(* parentheses around a content *)
let parens (content) :=
  | LEFT_PARENS; expr = content; RIGHT_PARENS; { expr }
(* braces around a content *)
let braces (content) :=
  | LEFT_BRACES; expr = content; RIGHT_BRACES; { expr }
(* brackets around a content *)
let brackets (content) :=
  | LEFT_BRACKET; expr = content; RIGHT_BRACKET; { expr }

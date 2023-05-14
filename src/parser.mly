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

  let replace_type_constants_with_variables variables pretype =
    let name_map = Hashtbl.create 12 in
    let vars_ref = ref [] in
    List.iter (fun var_name -> Hashtbl.replace name_map var_name None) variables ;
    let rec f = function
      | Ptyp_const { name } as ty -> (
          try
            match Hashtbl.find name_map name with
            | Some var -> var
            | None ->
                let var_id, var = Typer.Skolem.make_bound () in
                vars_ref := var_id :: !vars_ref ;
                Hashtbl.replace name_map name (Some var) ;
                var
          with
          | Not_found -> ty)
      | Ptyp_var _ as ty -> ty
      | Ptyp_apply { base; argm } ->
          let base = f base in
          let argm = f argm in
          Ptyp_apply { base; argm }
      | Ptyp_arrow { param; return } ->
          let param = f param in
          let return = f return in
          Ptyp_arrow { param; return }
      | Ptyp_forall { param; return } ->
          let return = f return in
          Ptyp_forall { param; return }
    in
    List.rev !vars_ref, f pretype

  let handle_forall params body =
    let (variables, pretype) = replace_type_constants_with_variables params body in
    match variables with
    | [] -> pretype
    | xs -> Ptyp_forall { param = xs; return = pretype }
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
%%

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

let type_variable ==
  | value = LOWER; { Ptyp_const { name = Name.make value } }
let type_arrow ==
  | param = type_simple; ARROW; return = type_entry; { Ptyp_arrow { param; return } }
let type_forall ==
  | FORALL; params = braces(non_empty_list(COMMA, LOWER)); FATARROW; return = type_entry; { handle_forall params return }
let type_parens ==
  | LEFT_PARENS; value = type_entry; RIGHT_PARENS; { value } 

let non_empty_list(sep, expr) :=
  | init = expr; { [init] }
  | init = expr; sep; rest = separated_nonempty_list(sep, expr); { init :: rest }

let parens(content) :=
  | LEFT_PARENS; expr = content; RIGHT_PARENS; { expr }
let braces(content) :=
  | LEFT_BRACES; expr = content; RIGHT_BRACES; { expr }
let brackets(content) :=
  | LEFT_BRACKET; expr = content; RIGHT_BRACKET; { expr }

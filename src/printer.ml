open Tree
open Name
open Format

let sequence sep pp vs ppf = 
  let pp_sep ppf () = fprintf ppf sep in
  fprintf ppf "[%a]" (pp_print_list ~pp_sep pp) vs

let rec pp_expr fmt expr =
  match expr with
  | Pexp_annot { value; annot } ->
      fprintf fmt "(%a : %a)" pp_expr value pp_type annot
  | Pexp_lower { value } ->
      fprintf fmt "%s" (repr value)
  | Pexp_let { name; bind; body } ->
      fprintf fmt "let %s = %a; %a" (repr name) pp_expr bind pp_expr body
  | Pexp_lambda { param; annot = None; body } ->
      fprintf fmt "lambda %s => %a" (repr param) pp_expr body
  | Pexp_lambda { param; annot = Some annot; body } ->
      fprintf fmt "lambda (%s : %a) => %a" (repr param) pp_type annot pp_expr body
  | Pexp_apply { lambda; argm } ->
      fprintf fmt "%a(%a)" pp_expr lambda pp_expr argm

and pp_type fmt ty =
  match ty with
  | Ptyp_var name  -> 
      fprintf fmt "%a" pp_var name 
  | Ptyp_const { name } ->
      fprintf fmt "%s" (repr name) 
  | Ptyp_forall { param; return } ->
      fprintf fmt "(forall %t => %a)" (sequence ", " pp_id param) pp_type return
  | Ptyp_arrow { param = Ptyp_arrow _ as param; return } ->
      fprintf fmt "(%a) -> %a" pp_type param pp_type return
  | Ptyp_arrow { param; return } ->
      fprintf fmt "%a -> %a" pp_type param pp_type return
  | Ptyp_apply { base; argm } ->
      fprintf fmt "%a %a" pp_type base pp_type argm

and pp_var fmt var =
  match !var with
  | Unbound (name, _) ->   
      fprintf fmt "%d" name
  | Link link ->
      fprintf fmt "%a" pp_type link
  | Generic name ->
      fprintf fmt "%d" name
  | Bound name ->
      fprintf fmt "%d" name
open Tree
open Name
open Typer
open Format

let sequence sep pp vs ppf =
  let pp_sep ppf () = fprintf ppf sep in
  fprintf ppf "%a" (pp_print_list ~pp_sep pp) vs

let rec pp_expr fmt expr =
  match expr with
  | Pexp_literal { value } ->
      fprintf fmt "%a" pp_literal value 
  | Pexp_lower { value } -> 
      fprintf fmt "%a" pp_name value
  | Pexp_let { name; bind; body } ->
      fprintf fmt "let %a = %a; %a" pp_name name pp_expr bind pp_expr body
  | Pexp_tuple { values } -> 
      fprintf fmt "(%t)" (sequence ", " pp_expr values)
  | Pexp_annot { value; annot } ->
      fprintf fmt "(%a : %a)" pp_expr value pp_type annot
  | Pexp_lambda { param; body } ->
      fprintf fmt "lambda %a => %a" pp_patt param pp_expr body
  | Pexp_apply { lambda; argm } ->
      fprintf fmt "%a(%a)" pp_expr lambda pp_expr argm

and pp_literal fmt lit =
  match lit with
  | Plit_number { value } ->
      fprintf fmt "%d" value
  | Plit_unit ->
      fprintf fmt "()"
    
and pp_patt fmt patt =
  match patt with
  | Ppat_wild -> 
      fprintf fmt "_"
  | Ppat_var { name } ->
      fprintf fmt "%a" pp_name name
  | Ppat_annot { value; annot } ->
      fprintf fmt "(%a : %a)" pp_patt value pp_type annot

and pp_type fmt ty =
  match ty with
  | Ptyp_var { name } -> 
      fprintf fmt "%a" pp_name name
  | Ptyp_const { name } -> 
      fprintf fmt "%a" pp_name name
  | Ptyp_forall { param; return } ->
      fprintf fmt "(forall {%t} => %a)" (sequence ", " pp_name param) pp_type return
  | Ptyp_tuple { types } -> 
      fprintf fmt "(%t)" (sequence " * " pp_type types)
  | Ptyp_arrow { param = Ptyp_arrow _ as param; return } ->
      fprintf fmt "(%a) -> %a" pp_type param pp_type return
  | Ptyp_arrow { param; return } ->
      fprintf fmt "%a -> %a" pp_type param pp_type return
  | Ptyp_apply { base; argm } -> 
      fprintf fmt "%a %a" pp_type base pp_type argm

(* Typer Tree *)

let rec pp_texpr fmt expr =
  match expr with
  | TTree.ELiteral value ->
      fprintf fmt "%a" pp_tliteral value
  | TTree.EAnnot (value, annot) ->
      fprintf fmt "(%a : %a)" pp_texpr value pp_ttype annot
  | TTree.ELower value -> 
      fprintf fmt "%a" pp_name value
  | TTree.ELet (name, bind, body) ->
      fprintf fmt "let %a = %a; %a" pp_name name pp_texpr bind pp_texpr body
  | TTree.ETuple values ->
      fprintf fmt "(%t)" (sequence ", " pp_texpr values)
  | TTree.ELambda (param, body) ->
      fprintf fmt "lambda %a => %a" pp_patt param pp_texpr body
  | TTree.EApply (lambda, argm) ->
      fprintf fmt "%a(%a)" pp_texpr lambda pp_texpr argm

and pp_ttype fmt ty =
  match ty with
  | TTree.TVar name -> 
      fprintf fmt "%a" pp_tvar name
  | TTree.TPrim prim -> 
      fprintf fmt "%a" pp_tprim prim
  | TTree.TCon name -> 
      fprintf fmt "%a" pp_name name
  | TTree.TTuple types -> 
      fprintf fmt "(%t)" (sequence ", " pp_ttype types)
  | TTree.TForall (param, return) ->
      fprintf fmt "(forall {%t} => %a)" (sequence " * " pp_print_int param) pp_ttype return
  | TTree.TArrow ((TTree.TArrow _ as param), return) ->
      fprintf fmt "(%a) -> %a" pp_ttype param pp_ttype return
  | TTree.TArrow (param, return) ->
      fprintf fmt "%a -> %a" pp_ttype param pp_ttype return
  | TTree.TApply (base, argm) -> 
      fprintf fmt "%a %a" pp_ttype base pp_ttype argm

and pp_tliteral fmt lit =
  match lit with
  | TTree.LNumber value ->
      fprintf fmt "%d" value
  | TTree.LUnit ->
      fprintf fmt "()"

and pp_tvar fmt var =
  match !var with
  | TTree.Unbound (name, _) -> 
      fprintf fmt "%d" name
  | TTree.Link link -> 
      fprintf fmt "%a" pp_ttype link
  | TTree.Generic name -> 
      fprintf fmt "%d" name
  | TTree.Bound name -> 
      fprintf fmt "%d" name
  
and pp_tprim fmt prim =
  match prim with
  | TTree.PUnit -> 
      fprintf fmt "Unit"
  | TTree.PInt -> 
      fprintf fmt "Int"

and pp_patt fmt patt =
  match patt with
  | TTree.PWild -> 
      fprintf fmt "_"
  | TTree.PVar name ->
      fprintf fmt "%a" pp_name name
  | TTree.PAnnot (value, annot) ->
      fprintf fmt "(%a : %a)" pp_patt value pp_ttype annot
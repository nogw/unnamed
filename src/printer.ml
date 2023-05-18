open Tree
open Name
open Typer
open Format

let sequence sep pp vs ppf =
  let pp_sep ppf () = fprintf ppf sep in
  fprintf ppf "[%a]" (pp_print_list ~pp_sep pp) vs

let rec pp_expr fmt expr =
  match expr with
  | Pexp_annot { value; annot } ->
      fprintf fmt "(%a : %a)" pp_expr value pp_type annot
  | Pexp_lower { value } -> 
      fprintf fmt "%a" pp_name value
  | Pexp_let { name; bind; body } ->
      fprintf fmt "let %a = %a; %a" pp_name name pp_expr bind pp_expr body
  | Pexp_lambda { param; annot = None; body } ->
      fprintf fmt "lambda %a => %a" pp_name param pp_expr body
  | Pexp_lambda { param; annot = Some annot; body } ->
      fprintf fmt "lambda (%a : %a) => %a" pp_name param pp_type annot pp_expr body
  | Pexp_apply { lambda; argm } ->
      fprintf fmt "%a(%a)" pp_expr lambda pp_expr argm

and pp_type fmt ty =
  match ty with
  | Ptyp_var { name } -> 
      fprintf fmt "%a" pp_name name
  | Ptyp_const { name } -> 
      fprintf fmt "%a" pp_name name
  | Ptyp_forall { param; return } ->
      fprintf fmt "(forall %t => %a)" (sequence ", " pp_name param) pp_type return
  | Ptyp_tuple { types } -> 
      fprintf fmt "(%t)" (sequence ", " pp_type types)
  | Ptyp_arrow { param = Ptyp_arrow _ as param; return } ->
      fprintf fmt "(%a) -> %a" pp_type param pp_type return
  | Ptyp_arrow { param; return } ->
      fprintf fmt "%a -> %a" pp_type param pp_type return
  | Ptyp_apply { base; argm } -> 
      fprintf fmt "%a %a" pp_type base pp_type argm

(* Typer Tree *)

let pp_tprim fmt prim =
  match prim with
  | TTree.PUnit -> 
      fprintf fmt "()"
  | TTree.PInt -> 
      fprintf fmt "Int"

let rec pp_ttype fmt ty =
  match ty with
  | TTree.TVar name -> 
      fprintf fmt "%a" pp_var name
  | TTree.TPrim prim -> 
      fprintf fmt "%a" pp_tprim prim
  | TTree.TCon name -> 
      fprintf fmt "%a" pp_name name
  | TTree.TTuple types -> 
      fprintf fmt "(%t)" (sequence ", " pp_ttype types)
  | TTree.TForall (param, return) ->
      fprintf fmt "(forall %t => %a)" (sequence ", " pp_print_int param) pp_ttype return
  | TTree.TArrow ((TTree.TArrow _ as param), return) ->
      fprintf fmt "(%a) -> %a" pp_ttype param pp_ttype return
  | TTree.TArrow (param, return) ->
      fprintf fmt "%a -> %a" pp_ttype param pp_ttype return
  | TTree.TApply (base, argm) -> 
      fprintf fmt "%a %a" pp_ttype base pp_ttype argm

and pp_var fmt var =
  match !var with
  | TTree.Unbound (name, _) -> 
      fprintf fmt "%d" name
  | TTree.Link link -> 
      fprintf fmt "%a" pp_ttype link
  | TTree.Generic name -> 
      fprintf fmt "%d" name
  | TTree.Bound name -> 
      fprintf fmt "%d" name

let rec pp_texpr fmt expr =
  match expr with
  | TTree.EAnnot (value, annot) ->
      fprintf fmt "(%a : %a)" pp_texpr value pp_ttype annot
  | TTree.ELower value -> 
      fprintf fmt "%a" pp_name value
  | TTree.ELet (name, bind, body) ->
      fprintf fmt "let %a = %a; %a" pp_name name pp_texpr bind pp_texpr body
  | TTree.ELambda (param, None, body) ->
      fprintf fmt "lambda %a => %a" pp_name param pp_texpr body
  | TTree.ELambda (param, Some annot, body) ->
      fprintf fmt "lambda (%a : %a) => %a" pp_name param pp_ttype annot pp_texpr body
  | TTree.EApply (lambda, argm) ->
      fprintf fmt "%a(%a)" pp_texpr lambda pp_texpr argm
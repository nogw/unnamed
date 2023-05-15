module PTree = Tree

module TTree = struct
  (* TODO: move to file *)
  type name = Name.t [@@deriving show]
  type id = int [@@deriving show]

  type 'link var = Link of 'link | Bound of id | Generic of id | Unbound of id * Level.t
  [@@deriving show]

  type poly =
    | TCon of name
    | TVar of poly var ref
    | TArrow of poly * poly
    | TApply of poly * poly
    | TForall of id list * poly
  [@@deriving show]

  type expr =
    | ELower of name
    | ELet of name * expr * expr
    | ELambda of name * poly option * expr
    | EApply of expr * expr
    | EAnnot of expr * poly
  [@@deriving show]
end

module Param = struct
  (* TODO: move to printer? *)
  let counter = ref (-1)

  let from_int counter =
    let diff = counter mod 26 in
    let char = Char.chr (97 + diff) in
    let div = counter / 26 in
    match div with
    | 0 -> String.make 1 char
    | _ -> string_of_int div ^ String.make 1 char

  let next () =
    incr counter ;
    from_int !counter

  let reset () = counter := 0
end

module Show = struct
  (* TODO: move to printer *)
  module Vars = Map.Make (Int)

  type context = string Vars.t

  let generate_vars vars_map variables =
    let generate_var _ = Param.from_int (Vars.cardinal vars_map) in
    List.map generate_var variables

  let extend_vars_map vars_map variables vars_gen =
    let add_name vars_map var_id name = Vars.add var_id name vars_map in
    List.fold_left2 add_name vars_map variables vars_gen

  let extend_vars vars_map variables =
    let vars_gen = generate_vars vars_map variables in
    let vars_map = extend_vars_map vars_map variables vars_gen in
    List.rev vars_gen, vars_map

  let rec string_of_type vars pretype =
    match pretype with
    | TTree.TCon name -> name
    | TTree.TVar { contents = Link ty } -> string_of_type vars ty
    | TTree.TVar { contents = Unbound (id, _) } -> "?" ^ string_of_int id
    | TTree.TVar { contents = Generic id } -> "'" ^ string_of_int id
    | TTree.TVar { contents = Bound id } -> (
        match Vars.find_opt id vars with
        | Some name -> name
        | None -> "?" ^ string_of_int id)
    | TTree.TArrow (param, return) ->
        let param_str = string_of_type vars param in
        let return_str = string_of_type vars return in
        Format.sprintf "%s -> %s" param_str return_str
    | TTree.TApply (base, argm) ->
        let base_str = string_of_type vars base in
        let argm_str = string_of_type vars argm in
        Format.sprintf "%s(%s)" base_str argm_str
    | TTree.TForall (param, return) ->
        let param_names, vars = extend_vars vars param in
        let param_str = String.concat " " param_names in
        let return_str = string_of_type vars return in
        Format.sprintf "forall [%s] => %s" param_str return_str

  let of_type pretype =
    let context = Vars.empty in
    string_of_type context pretype
end

type errors =
  | RecursiveTypes
  | UnboundedId of int
  | UnboundedVariable of Name.t
  | UnboundedTypeAlias of Name.t
  | CannotUnify of TTree.poly * TTree.poly
  | CannotSubsume of TTree.poly * TTree.poly
  | ExpectFunction of TTree.poly
  | ExpectMonotype

exception TError of errors

let ( let* ) = Result.bind

module Skolem = struct
  open TTree

  module Fresh = struct
    let current = ref 0

    let next () =
      let id = !current in
      current := succ id ;
      id

    let reset () = current := 0
  end

  let make_var ~level =
    let make id = TVar (ref (Unbound (id, level))) in
    make (Fresh.next ())

  let make_gen () =
    let make id = TVar (ref (Generic id)) in
    make (Fresh.next ())

  let make_bound () =
    let make id = id, TVar (ref (Bound id)) in
    make (Fresh.next ())

  let fresh_vars ~level n = List.init n (fun _ -> make_var ~level)
  let fresh_bounds n = List.init n (fun _ -> make_bound ())
  let fresh_generics n = List.init n (fun _ -> make_gen ())
end

module Environment = struct
  (* TODO: i don't like this module
     - use another approach instead of recording
     - move to file *)
  module IMap = Map.Make (Int)
  module SMap = Map.Make (String)

  type t = {
    id : TTree.poly IMap.t; (* id variable *)
    alias : TTree.poly SMap.t; (* alias type *)
    infer : TTree.poly SMap.t; (* infer type *)
  }

  let empty = { id = IMap.empty; alias = SMap.empty; infer = SMap.empty }
  let extend_alias ~name alias context = { context with alias = SMap.add name alias context.alias }
  let extend_infer ~name infer context = { context with infer = SMap.add name infer context.infer }

  let extend_list_id keys values =
    let aux map key value = IMap.add key value map in
    List.fold_left2 aux empty.id keys values

  let remove_list_id map keys =
    let aux map key = IMap.remove key map in
    List.fold_left aux map keys

  let lookup_id ~id env = IMap.find_opt id env
  let lookup_alias ~name context = SMap.find_opt name context.alias
  let lookup_infer ~name context = SMap.find_opt name context.infer
end

module Transl = struct
  module Tbl = Hashtbl

  module Context = struct
    type context = (string, TTree.poly option) Tbl.t
    type t = context

    let create ~init : t = Tbl.create init
    let extend ~name value env = Tbl.replace env name value
    let lookup ~name env = Tbl.find env name

    let from_variables ?(init = 16) variables =
      let context = create ~init in
      let addNone name = extend ~name None context in
      List.iter addNone variables ;
      context
  end

  type state = { context : Context.t; entries : int list ref; environment : Environment.t }

  let enter_skolem ~state name =
    let unique, bound = Skolem.make_bound () in
    let () = Context.extend ~name (Some bound) state.context in
    let () = state.entries := unique :: !(state.entries) in
    bound

  (* TODO: doesn't look good *)
  let rec transl_wrapper ?(level = 0) ?(variables = []) environment pretype =
    let context = Context.from_variables variables in
    let entries = ref [] in
    let pretype =
      transl_type ~state:{ context; entries; environment } ~env:environment ~level pretype
    in
    List.rev !entries, pretype

  and transl_type ~state ~env ~level pretype =
    let transl = transl_type ~state ~env in
    match pretype with
    | PTree.Ptyp_var { name } -> (
        let desc = Context.lookup ~name state.context in
        match desc with
        | Some ty -> ty
        | None -> enter_skolem ~state name)
    | PTree.Ptyp_arrow { param; return } ->
        let param = transl ~level param in
        let return = transl ~level return in
        TTree.TArrow (param, return)
    | PTree.Ptyp_apply { base; argm } ->
        let base = transl ~level base in
        let argm = transl ~level argm in
        TTree.TApply (base, argm)
    | PTree.Ptyp_forall { param; return } -> (
        let level = Level.next level in
        match transl_wrapper ~level ~variables:param env return with
        | [], return -> return
        | xs, return -> TTree.TForall (xs, return))
    | PTree.Ptyp_const { name } -> (
        let desc = Environment.lookup_alias ~name env in
        match desc with
        | Some ty -> ty
        | None -> raise (TError (UnboundedTypeAlias name)))

  and transl_expr ~env expr =
    let transl = transl_expr ~env in
    match expr with
    | PTree.Pexp_lower { value } -> TTree.ELower value
    | PTree.Pexp_annot { value; annot } ->
        let value = transl value in
        let _, annot = transl_wrapper env annot in
        TTree.EAnnot (value, annot)
    | PTree.Pexp_let { name; bind; body } ->
        let bind = transl bind in
        let body = transl body in
        TTree.ELet (name, bind, body)
    | PTree.Pexp_apply { lambda; argm } ->
        let lambda = transl lambda in
        let argm = transl argm in
        TTree.EApply (lambda, argm)
    | PTree.Pexp_lambda { param; annot = Some annot; body } ->
        let _, annot = transl_wrapper env annot in
        let body = transl body in
        TTree.ELambda (param, Some annot, body)
    | PTree.Pexp_lambda { param; annot = None; body } ->
        let body = transl body in
        TTree.ELambda (param, None, body)
end

module Occurs = struct
  module Set = struct
    type 'a t = ('a, unit) Hashtbl.t

    let create n : 'a t = Hashtbl.create n
    let add set el = Hashtbl.replace set el ()
    let mem set el = Hashtbl.mem set el
  end

  let generics = Set.create 12

  let rec free_variables pretype =
    match pretype with
    | TTree.TCon _ -> ()
    | TTree.TVar { contents = Bound _ } -> ()
    | TTree.TVar { contents = Unbound _ } -> ()
    | TTree.TVar { contents = Generic _ } as ty -> Set.add generics ty
    | TTree.TVar { contents = Link link } -> free_variables link
    | TTree.TArrow (param, return) ->
        free_variables param ;
        free_variables return
    | TTree.TApply (base, argm) ->
        free_variables base ;
        free_variables argm
    | TTree.TForall (_, return) -> free_variables return

  let freevars_wrapper pretype =
    let () = free_variables pretype in
    generics

  let escape_check generics lhs rhs =
    let free_lhs = freevars_wrapper lhs in
    let free_rhs = freevars_wrapper rhs in
    List.exists (fun v -> Set.mem free_lhs v || Set.mem free_rhs v) generics
end

module Subst = struct
  let subst variables types pretype =
    let rec apply variables pretype =
      match pretype with
      | TTree.TCon _ -> Ok pretype
      | TTree.TVar { contents = Link link } -> apply variables link
      | TTree.TVar { contents = Bound var } -> (
          match Environment.lookup_id ~id:var variables with
          | Some ty -> Ok ty
          | None -> Error (UnboundedId var))
      | TTree.TVar _ as variable -> Ok variable
      | TTree.TApply (base, argm) ->
          let* base = apply variables base in
          let* argm = apply variables argm in
          Ok (TTree.TApply (base, argm))
      | TTree.TArrow (param, return) ->
          let* param = apply variables param in
          let* return = apply variables return in
          Ok (TTree.TArrow (param, return))
      | TTree.TForall (param, return) ->
          let removed = Environment.remove_list_id variables param in
          let* return = apply removed return in
          Ok (TTree.TForall (param, return))
    in
    apply (Environment.extend_list_id variables types) pretype

  let with_variables level variables pretype =
    let fresh = Skolem.fresh_vars ~level (List.length variables) in
    let* poly = subst variables fresh pretype in
    Ok (fresh, poly)
end

let rec repr pretype =
  match pretype with
  | TTree.TVar ({ contents = Link link } as var) ->
      let con = repr link in
      var := Link con ;
      con
  | _ -> pretype

let rec is_annotated expr =
  match expr with
  | TTree.ELet (_, _, body) -> is_annotated body
  | TTree.EAnnot _ -> true
  | _ -> false

let rec is_monomorphic pretype =
  match repr pretype with
  | TTree.TCon _ -> true
  | TTree.TVar _ -> true
  | TTree.TForall _ -> false
  | TTree.TArrow (param, return) ->
      let param = is_monomorphic param in
      let return = is_monomorphic return in
      param && return
  | TTree.TApply (base, argm) ->
      let base = is_monomorphic base in
      let argm = is_monomorphic argm in
      base && argm

let occurs_check_adjust_levels id level pretype =
  let rec apply pretype =
    let check_var var =
      match !var with
      | TTree.Link link -> apply link
      | TTree.Unbound (idu, _) when id = idu -> Error RecursiveTypes
      | TTree.Unbound (_, lvu) when level < lvu -> Ok ()
      | TTree.Unbound (idu, _) -> Ok (var := Unbound (idu, level))
      | _ -> Ok ()
    in
    match pretype with
    | TTree.TCon _ -> Ok ()
    | TTree.TVar v -> check_var v
    | TTree.TArrow (param, return) ->
        let* () = apply param in
        let* () = apply return in
        Ok ()
    | TTree.TApply (base, argm) ->
        let* () = apply base in
        let* () = apply argm in
        Ok ()
    | TTree.TForall (_, return) ->
        let* () = apply return in
        Ok ()
  in
  apply pretype

let rec unify lhs rhs =
  match lhs, rhs with
  | lhs, rhs when lhs == rhs -> Ok ()
  | TTree.TVar { contents = Bound _ }, _ -> assert false
  | _, TTree.TVar { contents = Bound _ } -> assert false
  | TTree.TVar { contents = Link lhs }, rhs
  | lhs, TTree.TVar { contents = Link rhs } ->
      unify lhs rhs
  | TTree.TVar ({ contents = Unbound (id, level) } as tvar), ty
  | ty, TTree.TVar ({ contents = Unbound (id, level) } as tvar) ->
      let* () = occurs_check_adjust_levels id level ty in
      tvar := Link ty ;
      Ok ()
  | TTree.TApply (l1, a1), TTree.TApply (l2, r2) ->
      let* () = unify l1 l2 in
      let* () = unify a1 r2 in
      Ok ()
  | TTree.TArrow (p1, r1), TTree.TArrow (p2, r2) ->
      let* () = unify p1 p2 in
      let* () = unify r1 r2 in
      Ok ()
  | TTree.TForall (p1, r1), TTree.TForall (p2, r2) ->
      let llen = List.length p1 in
      let rlen = List.length p2 in
      if llen = rlen then
        let generics = Skolem.fresh_generics llen in
        let* gen_lhs = Subst.subst p1 generics r1 in
        let* gen_rhs = Subst.subst p2 generics r2 in
        let* () = unify gen_lhs gen_rhs in
        match Occurs.escape_check generics lhs rhs with
        | true -> Error (CannotUnify (lhs, rhs))
        | false -> Ok ()
      else Error (CannotUnify (lhs, rhs))
  | lhs, rhs -> Error (CannotUnify (lhs, rhs))

let instantiate_annotation level forall =
  match forall with
  | [], pretype -> Ok ([], pretype)
  | is, pretype -> Subst.with_variables level is pretype

let instantiate level pretype =
  match repr pretype with
  | TTree.TForall (param, return) ->
      let* _, substitution = Subst.with_variables level param return in
      Ok substitution
  | pretype -> Ok pretype

let subsume level lhs rhs =
  let* instantiated_rhs = instantiate level rhs in
  match repr lhs with
  | TTree.TForall (param, return) as forall -> (
      let gen_params = Skolem.fresh_generics (List.length param) in
      let* gen_return = Subst.subst param gen_params return in
      let* () = unify gen_return instantiated_rhs in
      match Occurs.escape_check gen_params forall rhs with
      | true -> Error (CannotSubsume (forall, rhs))
      | false -> Ok ())
  | lhs -> unify lhs instantiated_rhs

let generalize level pretype =
  let variables = ref [] in
  let rec apply pretype =
    match pretype with
    | TTree.TCon _ -> ()
    | TTree.TVar { contents = Link link } -> apply link
    | TTree.TVar { contents = Generic _ } -> assert false
    | TTree.TVar { contents = Bound _ } -> ()
    | TTree.TVar ({ contents = Unbound (id, lvl) } as var) when lvl > level -> (
        var := Bound id ;
        match List.mem id !variables with
        | true -> ()
        | false -> variables := id :: !variables)
    | TTree.TVar { contents = Unbound _ } -> ()
    | TTree.TApply (base, argm) ->
        apply base ;
        apply argm
    | TTree.TArrow (param, return) ->
        apply param ;
        apply return
    | TTree.TForall (_, return) -> apply return
  in
  let () = apply pretype in
  match !variables with
  | [] -> pretype
  | vs -> TTree.TForall (List.rev vs, pretype)

let destruct_lambda_type pretype =
  match repr pretype with
  | TTree.TArrow (param, return) -> Ok (param, return)
  | TTree.TVar ({ contents = Unbound (_, level) } as tvar) ->
      let param = Skolem.make_var ~level in
      let return = Skolem.make_var ~level in
      tvar := Link (TTree.TArrow (param, return)) ;
      Ok (param, return)
  | _ -> Error (ExpectFunction pretype)

let rec infer ~env level pretype =
  match pretype with
  | TTree.ELower value -> (
      match Environment.lookup_infer ~name:value env with
      | Some pretype -> Ok pretype
      | None -> Error (UnboundedVariable value))
  | TTree.EAnnot (value, annot) ->
      let* _, anno = instantiate_annotation level ([], annot) in
      let* infered = infer ~env level value in
      let* () = subsume level anno infered in
      Ok anno
  | TTree.ELet (name, bind, body) ->
      let* bind = infer ~env (Level.next level) bind in
      infer ~env:(Environment.extend_infer ~name bind env) level body
  | TTree.EApply (lambda, argm) ->
      let* infer_lambda = infer ~env (Level.next level) lambda in
      let* insta_lambda = instantiate (Level.next level) infer_lambda in
      let* param, return = destruct_lambda_type insta_lambda in
      let* () = infer_args ~env (Level.next level) param argm in
      let* return = instantiate (Level.next level) return in
      Ok (generalize level return)
  | TTree.ELambda (param, annot, body) -> (
      let context_ref = ref env in
      let variables = ref [] in
      let* param_type =
        match annot with
        | None ->
            let var = Skolem.make_var ~level:(Level.next level) in
            variables := var :: !variables ;
            Ok var
        | Some annot ->
            let* vars, anno = instantiate_annotation (Level.next level) ([], annot) in
            variables := vars @ !variables ;
            Ok anno
      in
      context_ref := Environment.extend_infer ~name:param param_type !context_ref ;
      let* infer_return = infer ~env:!context_ref (Level.next level) body in
      let* insta_return =
        match is_annotated body, infer_return with
        | true, _ -> Ok infer_return
        | false, _ -> instantiate (Level.next level) infer_return
      in
      match List.for_all is_monomorphic !variables with
      | true -> Ok (generalize level (TTree.TArrow (param_type, insta_return)))
      | false -> Error ExpectMonotype)

and infer_args ~env level param_ty arg_expr =
  let* arg_ty = infer ~env level arg_expr in
  match is_annotated arg_expr with
  | true -> unify param_ty arg_ty
  | false -> subsume level param_ty arg_ty

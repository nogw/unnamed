open Tree

let ( let* ) = Result.bind

type errors =
  | RecursiveTypes
  | UnboundedId of int
  | UnboundedVariable of Name.t
  | CannotUnify of Tree.poly * Tree.poly
  | CannotSubsume of Tree.poly * Tree.poly
  | ExpectFunction of Tree.poly
  | ExpectMonotype

exception CheckError of errors

let rec repr pretype =
  match pretype with
  | Ptyp_var ({ contents = Link link } as var) ->
      let con = repr link in
      var := Link con ;
      con
  | _ -> pretype

let rec is_annotated expr =
  match expr with
  | Pexp_let { body; _ } -> is_annotated body
  | Pexp_annot _ -> true
  | _ -> false

let rec is_monomorphic pretype =
  match repr pretype with
  | Ptyp_const _ -> true
  | Ptyp_var _ -> true
  | Ptyp_forall _ -> false
  | Ptyp_arrow { param; return } ->
      let param = is_monomorphic param in
      let return = is_monomorphic return in
      param && return
  | Ptyp_apply { base; argm } ->
      let base = is_monomorphic base in
      let argm = is_monomorphic argm in
      base && argm

module Param = struct
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

module Skolem = struct
  module Fresh = struct
    let current = ref 0

    let next () =
      let id = !current in
      current := succ id ;
      id

    let reset () = current := 0
  end

  let make_var ~level =
    let make id = Ptyp_var (ref (Unbound (id, level))) in
    make (Fresh.next ())

  let make_gen () =
    let make id = Ptyp_var (ref (Generic id)) in
    make (Fresh.next ())

  let make_bound () =
    let make id = id, Ptyp_var (ref (Bound id)) in
    make (Fresh.next ())

  let fresh_generics n = List.init n (fun _ -> make_gen ())
  let fresh_bounds n = List.init n (fun _ -> make_bound ())
end

module Context = struct
  module IntMap = Map.Make (Int)
  module NameMap = Map.Make (Name)

  type id_env = Tree.poly IntMap.t
  type name_env = Tree.poly NameMap.t

  let empty_id = IntMap.empty
  let empty_name = NameMap.empty
  let extend_name ~name ~poly env = NameMap.add name poly env
  let lookup_id ~id env = IntMap.find_opt id env
  let lookup_name ~name env = NameMap.find_opt name env

  let extend_list_id keys values =
    let aux map key value = IntMap.add key value map in
    List.fold_left2 aux empty_id keys values

  let remove_list_id map keys =
    let aux map key = IntMap.remove key map in
    List.fold_left aux map keys
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
    | Tree.Ptyp_const _ -> ()
    | Tree.Ptyp_var { contents = Bound _ } -> ()
    | Tree.Ptyp_var { contents = Unbound _ } -> ()
    | Tree.Ptyp_var { contents = Generic _ } as ty -> Set.add generics ty
    | Tree.Ptyp_var { contents = Link link } -> free_variables link
    | Tree.Ptyp_arrow { param; return } ->
        free_variables param ;
        free_variables return
    | Tree.Ptyp_apply { base; argm } ->
        free_variables base ;
        free_variables argm
    | Tree.Ptyp_forall { return; _ } -> free_variables return

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
      | Ptyp_const _ -> Ok pretype
      | Ptyp_var { contents = Link link } -> apply variables link
      | Ptyp_var { contents = Bound var } -> (
          match Context.lookup_id ~id:var variables with
          | Some ty -> Ok ty
          | None -> Error (UnboundedId var))
      | Ptyp_var _ as variable -> Ok variable
      | Ptyp_apply { base; argm } ->
          let* base = apply variables base in
          let* argm = apply variables argm in
          Ok (Ptyp_apply { base; argm })
      | Ptyp_arrow { param; return } ->
          let* param = apply variables param in
          let* return = apply variables return in
          Ok (Ptyp_arrow { param; return })
      | Ptyp_forall { param; return } ->
          let removed = Context.remove_list_id variables param in
          let* return = apply removed return in
          Ok (Ptyp_forall { param; return })
    in
    apply (Context.extend_list_id variables types) pretype

  let with_variables level variables pretype =
    let types = List.rev_map (fun _ -> Skolem.make_var ~level) variables in
    let* poly = subst variables types pretype in
    Ok (types, poly)
end

let occurs_check_adjust_levels id level pretype =
  let rec apply pretype =
    let check_var var =
      match !var with
      | Link link -> apply link
      | Unbound (idu, _) when id = idu -> Error RecursiveTypes
      | Unbound (_, lvu) when level < lvu -> Ok ()
      | Unbound (idu, _) -> Ok (var := Unbound (idu, level))
      | _ -> Ok ()
    in
    match pretype with
    | Ptyp_const _ -> Ok ()
    | Ptyp_var v -> check_var v
    | Ptyp_arrow { param; return } ->
        let* () = apply param in
        let* () = apply return in
        Ok ()
    | Ptyp_forall { return; _ } ->
        let* () = apply return in
        Ok ()
    | Ptyp_apply { base; argm } ->
        let* () = apply base in
        let* () = apply argm in
        Ok ()
  in
  apply pretype

let rec unify lhs rhs =
  match lhs, rhs with
  | lhs, rhs when lhs == rhs -> Ok ()
  | Ptyp_var { contents = Link lhs }, rhs -> unify lhs rhs
  | lhs, Ptyp_var { contents = Link rhs } -> unify lhs rhs
  | Ptyp_var { contents = Bound _ }, _ -> assert false
  | _, Ptyp_var { contents = Bound _ } -> assert false
  | Ptyp_var ({ contents = Unbound (id, level) } as tvar), ty
  | ty, Ptyp_var ({ contents = Unbound (id, level) } as tvar) ->
      let* () = occurs_check_adjust_levels id level ty in
      Ok (tvar := Link ty)
  | Ptyp_apply lhs, Ptyp_apply rhs ->
      let* () = unify lhs.base rhs.base in
      let* () = unify lhs.argm rhs.argm in
      Ok ()
  | Ptyp_arrow lhs, Ptyp_arrow rhs ->
      let* () = unify lhs.param rhs.param in
      let* () = unify lhs.return rhs.return in
      Ok ()
  | Ptyp_forall lf, Ptyp_forall rf ->
      let llen = List.length lf.param in
      let rlen = List.length rf.param in
      if llen = rlen then
        let generics = Skolem.fresh_generics llen in
        let* gen_lhs = Subst.subst lf.param generics lf.return in
        let* gen_rhs = Subst.subst rf.param generics rf.return in
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
  | Ptyp_forall { param; return } ->
      let* _, substitution = Subst.with_variables level param return in
      Ok substitution
  | pretype -> Ok pretype

let subsume level lhs rhs =
  let* instantiated_rhs = instantiate level rhs in
  match repr lhs with
  | Ptyp_forall { param; return } as forall -> (
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
    | Ptyp_const _ -> ()
    | Ptyp_var { contents = Link link } -> apply link
    | Ptyp_var { contents = Generic _ } -> assert false
    | Ptyp_var { contents = Bound _ } -> ()
    | Ptyp_var ({ contents = Unbound (id, lvl) } as var) when lvl > level -> (
        var := Bound id ;
        match List.mem id !variables with
        | true -> ()
        | false -> variables := id :: !variables)
    | Ptyp_var { contents = Unbound _ } -> ()
    | Ptyp_apply { base; argm } ->
        apply base ;
        apply argm
    | Ptyp_arrow { param; return } ->
        apply param ;
        apply return
    | Ptyp_forall { return; _ } -> apply return
  in
  let () = apply pretype in
  match !variables with
  | [] -> pretype
  | vs -> Ptyp_forall { param = List.rev vs; return = pretype }

let destruct_lambda_type pretype =
  match repr pretype with
  | Ptyp_arrow { param; return } -> Ok (param, return)
  | Ptyp_var ({ contents = Unbound (_, level) } as tvar) ->
      let param = Skolem.make_var ~level in
      let return = Skolem.make_var ~level in
      tvar := Link (Ptyp_arrow { param; return }) ;
      Ok (param, return)
  | _ -> Error (ExpectFunction pretype)

let rec infer ~ctx level pretype =
  match pretype with
  | Pexp_lower { value } -> (
      match Context.lookup_name ~name:value ctx with
      | Some pretype -> Ok pretype
      | None -> Error (UnboundedVariable value))
  | Pexp_annot { value; annot } ->
      let* _, anno = instantiate_annotation level ([], annot) in
      let* infered = infer ~ctx level value in
      let* () = subsume level anno infered in
      Ok anno
  | Pexp_let { name; bind; body } ->
      let* bind = infer ~ctx (Level.next level) bind in
      infer ~ctx:(Context.extend_name ~name ~poly:bind ctx) level body
  | Pexp_apply { lambda; argm } ->
      let* infer_lambda = infer ~ctx (Level.next level) lambda in
      let* insta_lambda = instantiate (Level.next level) infer_lambda in
      let* param, return = destruct_lambda_type insta_lambda in
      let* () = infer_args ~ctx (Level.next level) param argm in
      let* return = instantiate (Level.next level) return in
      Ok (generalize level return)
  | Pexp_lambda { param; annot; body } -> (
      let context_ref = ref ctx in
      let variables = ref [] in
      let* param_type =
        match annot with
        | None ->
            let var = Skolem.make_var ~level:(Level.next level) in
            variables := var :: !variables ;
            Ok var
        | Some annot ->
            let* vars, anno =
              instantiate_annotation (Level.next level) ([], annot)
            in
            variables := vars @ !variables ;
            Ok anno
      in
      context_ref
        := Context.extend_name ~name:param ~poly:param_type !context_ref ;
      let* infer_return = infer ~ctx:!context_ref (Level.next level) body in
      let* insta_return =
        match is_annotated body, infer_return with
        | true, _ -> Ok infer_return
        | false, _ -> instantiate (Level.next level) infer_return
      in
      match List.for_all is_monomorphic !variables with
      | true ->
          Ok
            (generalize
               level
               (Ptyp_arrow { param = param_type; return = insta_return }))
      | false -> Error ExpectMonotype)

and infer_args ~ctx level param_ty arg_expr =
  let* arg_ty = infer ~ctx level arg_expr in
  match is_annotated arg_expr with
  | true -> unify param_ty arg_ty
  | false -> subsume level param_ty arg_ty

module Show = struct
  (* TODO: move to printer *)
  module Vars = Map.Make (Int)

  type context = string Vars.t

  let extend_vars name_map var_id_list =
    let name_list_rev, name_map =
      List.fold_left
        (fun (name_list, name_map) var_id ->
          let new_name = Param.from_int (Vars.cardinal name_map) in
          new_name :: name_list, Vars.add var_id new_name name_map)
        ([], name_map)
        var_id_list
    in
    List.rev name_list_rev, name_map

  let rec string_of_type vars pretype =
    let string_of_var id =
      match Vars.find_opt id vars with
      | Some name -> name
      | None -> "?" ^ string_of_int id
    in
    match pretype with
    | Ptyp_var { contents = Unbound (id, _) } -> string_of_var id
    | Ptyp_var { contents = Bound id } -> string_of_var id
    | Ptyp_var { contents = Generic id } -> "'" ^ string_of_int id
    | Ptyp_var { contents = Link ty } -> string_of_type vars ty
    | Ptyp_arrow { param; return } ->
        let param_str = string_of_type vars param in
        let return_str = string_of_type vars return in
        Format.sprintf "%s -> %s" param_str return_str
    | Ptyp_apply { base; argm } ->
        let base_str = string_of_type vars base in
        let argm_str = string_of_type vars argm in
        Format.sprintf "%s(%s)" base_str argm_str
    | Ptyp_forall { param; return } ->
        let param_names, vars = extend_vars vars param in
        let param_str = String.concat " " param_names in
        let return_str = string_of_type vars return in
        Format.sprintf "forall [%s] => %s" param_str return_str
    | Ptyp_const { name } -> name

  let of_type pretype =
    let context = Vars.empty in
    string_of_type context pretype
end

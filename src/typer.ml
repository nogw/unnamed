(* TODO: move most modules to separate files *)

module PTree = Tree

module TTree = struct
  type id = int [@@deriving show]
  type name = Name.t [@@deriving show]
  type prim = PInt | PUnit [@@deriving show { with_path = false }]

  type 'link var =
    | Link of 'link
    | Bound of id
    | Generic of id
    | Unbound of id * Level.t
  [@@deriving show { with_path = false }]

  type poly =
    | TCon of name
    | TVar of poly var ref
    | TPrim of prim
    | TTuple of poly list
    | TArrow of poly * poly
    | TApply of poly * poly
    | TForall of id list * poly
  [@@deriving show { with_path = false }]

  type literal = LUnit | LNumber of int
  [@@deriving show { with_path = false }]

  type patt = PWild | PVar of name | PAnnot of patt * poly
  [@@deriving show { with_path = false }]

  type expr =
    | ELiteral of literal
    | ELower of name
    | ELet of name * expr * expr
    | ETuple of expr list
    | ELambda of patt * expr
    | EApply of expr * expr
    | EAnnot of expr * poly
  [@@deriving show { with_path = false }]

  type term = TBind of { name : name; annot : poly option; body : expr }
  [@@deriving show { with_path = false }]

  let rec repr pretype =
    match pretype with
    | TVar ({ contents = Link link } as var) ->
        let con = repr link in
        var := Link con;
        con
    | _ -> pretype
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
    incr counter;
    from_int !counter

  let reset () = counter := 0
end

module Show = struct
  (* TODO: move to printer *)
  open Format
  module Vars = Map.Make (Int)

  type context = string Vars.t

  let extend_vars context variables =
    let generate_name context =
      let count = Vars.cardinal context in
      Param.from_int count
    in
    let extend_context context names id =
      let name = generate_name context in
      name :: names, Vars.add id name context
    in
    let extend (names, context) id = extend_context context names id in
    let names, context = List.fold_left extend ([], context) variables in
    List.rev names, context

  let string_of_prim prim =
    match prim with
    | TTree.PUnit -> "()"
    | TTree.PInt -> "Int"

  let rec string_of_type vars pretype =
    match TTree.repr pretype with
    | TTree.TCon name -> name
    | TTree.TPrim prim -> string_of_prim prim
    | TTree.TVar { contents = Link ty } -> string_of_type vars ty
    | TTree.TVar { contents = Unbound (id, _) } -> "?" ^ string_of_int id
    | TTree.TVar { contents = Generic id } -> "'" ^ string_of_int id
    | TTree.TVar { contents = Bound id } -> (
        match Vars.find_opt id vars with
        | Some name -> name
        | None -> "?" ^ string_of_int id)
    | TTree.TTuple types ->
        let types_str = List.map (string_of_type vars) types in
        sprintf "(%s)" (String.concat " * " types_str)
    | TTree.TArrow (param, return) -> (
        let param_str = string_of_type vars param in
        let return_str = string_of_type vars return in
        match TTree.repr param with
        | TTree.TArrow _ -> sprintf "(%s) -> %s" param_str return_str
        | _ -> sprintf "%s -> %s" param_str return_str)
    | TTree.TApply (base, argm) ->
        let base_str = string_of_type vars base in
        let argm_str = string_of_type vars argm in
        sprintf "%s(%s)" base_str argm_str
    | TTree.TForall (param, return) ->
        let param_names, vars = extend_vars vars param in
        let param_str = String.concat " " param_names in
        let return_str = string_of_type vars return in
        sprintf "forall {%s} => %s" param_str return_str

  let of_type pretype =
    let context = Vars.empty in
    string_of_type context pretype

  let pp_type fmt pretype = Format.fprintf fmt "%s" (of_type pretype)
end

module Skolem = struct
  open TTree

  module Fresh = struct
    let current = ref 0

    let next () =
      let id = !current in
      current := succ id;
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

type errors =
  | RecursiveTypes
  | UnboundedId of int
  | UnboundedVariable of Name.t
  | UnboundedTypeAlias of Name.t
  | CannotUnify of TTree.poly * TTree.poly
  | CannotSubsume of TTree.poly * TTree.poly
  | ExpectFunction of TTree.poly
  | ExpectMonotype
[@@deriving show]

exception TError of errors

module Environment = struct
  (* TODO: i don't like this module
     - use another approach instead of recording
     - move to file *)
  module IMap = Map.Make (Int)
  module SMap = Map.Make (String)

  type t = {
    id : TTree.poly IMap.t (* id variable *);
    alias : TTree.poly SMap.t (* alias type *);
    infer : TTree.poly SMap.t (* infer type *);
  }

  let empty = { id = IMap.empty; alias = SMap.empty; infer = SMap.empty }

  let extend_alias ~name alias context =
    { context with alias = SMap.add name alias context.alias }

  let extend_infer ~name infer context =
    { context with infer = SMap.add name infer context.infer }

  let extend_list_id keys values =
    let aux map key value = IMap.add key value map in
    List.fold_left2 aux IMap.empty keys values

  let remove_list_id map keys =
    let aux map key = IMap.remove key map in
    List.fold_left aux map keys

  let lookup_id ~id env = IMap.find_opt id env
  let lookup_alias ~name context = SMap.find_opt name context.alias
  let lookup_infer ~name context = SMap.find_opt name context.infer
end

module Core = struct
  type core_env = Environment.t

  (* TODO: f-omega hkt *)
  let initial_env =
    let empty_env = Environment.empty in
    let load_prim ~name prim env =
      let prim = TTree.TPrim prim in
      Environment.extend_alias ~name prim env
    in
    empty_env
    |> load_prim ~name:"Unit" TTree.PUnit
    |> load_prim ~name:"Int" TTree.PInt
end

module Transl = struct
  module Tbl = Hashtbl

  module Context = struct
    type 'value context = (string, 'value) Tbl.t
    type 'value t = 'value context

    let create ~init : 'value t = Tbl.create init
    let extend ~name value env = Tbl.replace env name value
    let lookup ~name env = Tbl.find env name

    let from_variables ?(init = 16) variables =
      let context = create ~init in
      let addNone name = extend ~name None context in
      List.iter addNone variables;
      context
  end

  type state = {
    environment : Environment.t;
    context : TTree.poly option Context.t;
    entries : int list ref;
  }

  let enter_skolem ~state name =
    let unique, bound = Skolem.make_bound () in
    let () = Context.extend ~name (Some bound) state.context in
    let () = state.entries := unique :: !(state.entries) in
    bound

  let create_ref () = ref []

  (* TODO: doesn't look good *)
  let rec transl_wrapper ?(level = 0) ?(variables = []) environment pretype =
    let context = Context.from_variables variables in
    let entries = create_ref () in
    let pretype =
      transl_type
        ~state:{ context; entries; environment }
        ~env:environment
        ~level
        pretype
    in
    List.rev !entries, pretype

  and transl_type ~state ~env ~level pretype =
    let transl = transl_type ~state ~env in
    match pretype with
    | PTree.Ptyp_const { name } -> (
        let desc = Environment.lookup_alias ~name env in
        match desc with
        | Some ty -> ty
        | None -> raise (TError (UnboundedTypeAlias name)))
    | PTree.Ptyp_tuple { types } ->
        let types = List.map (transl ~level) types in
        TTree.TTuple types
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

  and transl_patt ~env patt =
    match patt with
    | PTree.Ppat_wild -> TTree.PWild
    | PTree.Ppat_var { name } -> TTree.PVar name
    | PTree.Ppat_annot { value; annot } ->
        let value = transl_patt ~env value in
        let _, annot = transl_wrapper env annot in
        TTree.PAnnot (value, annot)

  (* TODO: useless? use parseTree? *)
  and transl_literal lit =
    match lit with
    | PTree.Plit_unit -> TTree.LUnit
    | PTree.Plit_number { value } -> TTree.LNumber value

  and transl_expr ~env expr =
    match expr with
    | PTree.Pexp_literal { value } ->
        let value = transl_literal value in
        TTree.ELiteral value
    | PTree.Pexp_lower { value } -> TTree.ELower value
    | PTree.Pexp_annot { value; annot } ->
        let value = transl_expr ~env value in
        let _, annot = transl_wrapper env annot in
        TTree.EAnnot (value, annot)
    | PTree.Pexp_let { name; bind; body } ->
        let bind = transl_expr ~env bind in
        let body = transl_expr ~env body in
        TTree.ELet (name, bind, body)
    | PTree.Pexp_tuple { values } ->
        let f expr = transl_expr ~env expr in
        let values = List.map f values in
        TTree.ETuple values
    | PTree.Pexp_apply { lambda; argm } ->
        let lambda = transl_expr ~env lambda in
        let argm = transl_expr ~env argm in
        TTree.EApply (lambda, argm)
    | PTree.Pexp_lambda { param; body } ->
        let param = transl_patt ~env param in
        let body = transl_expr ~env body in
        TTree.ELambda (param, body)

  and transl_term ~env term =
    match term with
    | PTree.PTerm_bind { name; annot = None; body } ->
        let body = transl_expr ~env body in
        TTree.TBind { name; annot = None; body }
    | PTree.PTerm_bind { name; annot = Some annot; body } ->
        let body = transl_expr ~env body in
        let _, annot = transl_wrapper env annot in
        TTree.TBind { name; annot = Some annot; body }
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
    | TTree.TPrim _ -> ()
    | TTree.TVar { contents = Bound _ } -> ()
    | TTree.TVar { contents = Unbound _ } -> ()
    | TTree.TVar { contents = Generic _ } as ty -> Set.add generics ty
    | TTree.TVar { contents = Link link } -> free_variables link
    | TTree.TTuple types -> List.iter free_variables types
    | TTree.TArrow (param, return) ->
        free_variables param;
        free_variables return
    | TTree.TApply (base, argm) ->
        free_variables base;
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
      | TTree.TCon _ -> pretype
      | TTree.TPrim _ -> pretype
      | TTree.TVar { contents = Link link } -> apply variables link
      | TTree.TVar { contents = Bound var } -> (
          match Environment.lookup_id ~id:var variables with
          | Some ty -> ty
          | None -> raise (TError (UnboundedId var)))
      | TTree.TVar _ as variable -> variable
      | TTree.TTuple types ->
          let types = List.map (apply variables) types in
          TTree.TTuple types
      | TTree.TApply (base, argm) ->
          let base = apply variables base in
          let argm = apply variables argm in
          TTree.TApply (base, argm)
      | TTree.TArrow (param, return) ->
          let param = apply variables param in
          let return = apply variables return in
          TTree.TArrow (param, return)
      | TTree.TForall (param, return) ->
          let removed = Environment.remove_list_id variables param in
          let return = apply removed return in
          TTree.TForall (param, return)
    in
    apply (Environment.extend_list_id variables types) pretype

  let with_variables level variables pretype =
    let fresh = Skolem.fresh_vars ~level (List.length variables) in
    let poly = subst variables fresh pretype in
    fresh, poly
end

let rec is_annotated expr =
  match expr with
  | TTree.ELet (_, _, body) -> is_annotated body
  | TTree.EAnnot _ -> true
  | _ -> false

let rec is_monomorphic pretype =
  match TTree.repr pretype with
  | TTree.TCon _ -> true
  | TTree.TPrim _ -> true
  | TTree.TVar _ -> true
  | TTree.TForall _ -> false
  | TTree.TTuple types ->
      let types = List.for_all is_monomorphic types in
      types
  | TTree.TArrow (param, return) ->
      let param = is_monomorphic param in
      let return = is_monomorphic return in
      param && return
  | TTree.TApply (base, argm) ->
      let base = is_monomorphic base in
      let argm = is_monomorphic argm in
      base && argm

let occurs_check_adjust_levels id level pretype =
  let rec check_var var =
    match var.contents with
    | TTree.Link ty -> apply ty
    | TTree.Unbound (id', _) when id' = id -> failwith "recursive types"
    | TTree.Unbound (id', lvl') when lvl' > level -> var := Unbound (id', level)
    | TTree.Unbound (_, _) -> ()
    | TTree.Generic _ -> ()
    | TTree.Bound _ -> ()
  and apply pretype =
    match pretype with
    | TTree.TCon _ -> ()
    | TTree.TPrim _ -> ()
    | TTree.TVar var -> check_var var
    | TTree.TTuple types -> List.iter apply types
    | TTree.TForall (_, return) -> apply return
    | TTree.TArrow (param, return) ->
        apply param;
        apply return
    | TTree.TApply (base, argm) ->
        apply base;
        apply argm
  in
  apply pretype

let rec unify lhs rhs =
  match lhs, rhs with
  | lhs, rhs when lhs == rhs -> ()
  | TTree.TPrim lhs, TTree.TPrim rhs when lhs = rhs -> ()
  | TTree.TVar { contents = Bound _ }, _ -> assert false
  | _, TTree.TVar { contents = Bound _ } -> assert false
  | TTree.TVar { contents = Link lhs }, rhs
  | lhs, TTree.TVar { contents = Link rhs } ->
      unify lhs rhs
  | TTree.TVar ({ contents = Unbound (id, lvl) } as var), ty
  | ty, TTree.TVar ({ contents = Unbound (id, lvl) } as var) ->
      occurs_check_adjust_levels id lvl ty;
      var := Link ty
  | TTree.TApply (l1, a1), TTree.TApply (l2, a2) ->
      unify l1 l2;
      unify a1 a2
  | TTree.TArrow (p1, r1), TTree.TArrow (p2, r2) ->
      unify p1 p2;
      unify r1 r2
  | TTree.TForall (p1, r1), TTree.TForall (p2, r2) ->
      let llen = List.length p1 in
      let rlen = List.length p2 in
      if llen = rlen then (
        let generics = Skolem.fresh_generics llen in
        let gen_lhs = Subst.subst p1 generics r1 in
        let gen_rhs = Subst.subst p2 generics r2 in
        unify gen_lhs gen_rhs;
        match Occurs.escape_check generics lhs rhs with
        | true -> raise (TError (CannotUnify (lhs, rhs)))
        | false -> ())
      else raise (TError (CannotUnify (lhs, rhs)))
  | lhs, rhs -> raise (TError (CannotUnify (lhs, rhs)))

let instantiate_annot level forall =
  match forall with
  | [], pretype -> [], pretype
  | is, pretype -> Subst.with_variables level is pretype

let instantiate level pretype =
  match TTree.repr pretype with
  | TTree.TForall (param, return) ->
      let _, substitution = Subst.with_variables level param return in
      substitution
  | ty -> ty

let subsume level lhs rhs =
  let instantiated_rhs = instantiate level rhs in
  match TTree.repr lhs with
  | TTree.TForall (param, return) as forall -> (
      let gen_params = Skolem.fresh_generics (List.length param) in
      let gen_return = Subst.subst param gen_params return in
      let () = unify gen_return instantiated_rhs in
      match Occurs.escape_check gen_params forall rhs with
      | true -> raise (TError (CannotSubsume (forall, rhs)))
      | false -> ())
  | lhs -> unify lhs instantiated_rhs

let generalize level pretype =
  let variables = ref [] in
  let rec apply pretype =
    match pretype with
    | TTree.TCon _ -> ()
    | TTree.TPrim _ -> ()
    | TTree.TVar { contents = Link link } -> apply link
    | TTree.TVar { contents = Generic _ } -> assert false
    | TTree.TVar { contents = Bound _ } -> ()
    | TTree.TVar ({ contents = Unbound (id, lvl) } as var) when lvl > level -> (
        var := Bound id;
        match List.mem id !variables with
        | true -> ()
        | false -> variables := id :: !variables)
    | TTree.TVar { contents = Unbound _ } -> ()
    | TTree.TTuple types -> List.iter apply types
    | TTree.TApply (base, argm) ->
        apply base;
        apply argm
    | TTree.TArrow (param, return) ->
        apply param;
        apply return
    | TTree.TForall (_, return) -> apply return
  in
  let () = apply pretype in
  match !variables with
  | [] -> pretype
  | vs -> TTree.TForall (List.rev vs, pretype)

let destruct_lambda_type pretype =
  match TTree.repr pretype with
  | TTree.TArrow (param, return) -> param, return
  | TTree.TVar ({ contents = Unbound (_, level) } as tvar) ->
      let param = Skolem.make_var ~level in
      let return = Skolem.make_var ~level in
      tvar := Link (TTree.TArrow (param, return));
      param, return
  | _ -> raise (TError (ExpectFunction pretype))

let infer_literal literal =
  (* TODO: avoid constructors *)
  match literal with
  | TTree.LNumber _ -> TTree.TPrim TTree.PInt
  | TTree.LUnit -> TTree.TPrim TTree.PUnit

let rec infer_patt ~env level patt =
  match patt with
  | TTree.PWild ->
      let var = Skolem.make_var ~level:(Level.next level) in
      var, [ var ], env
  | TTree.PVar name ->
      let var = Skolem.make_var ~level:(Level.next level) in
      let env = Environment.extend_infer ~name var env in
      var, [ var ], env
  | TTree.PAnnot (value, annot) ->
      let vars, anno = instantiate_annot level ([], annot) in
      let poly, vs, env = infer_patt ~env level value in
      let () = subsume level anno poly in
      anno, vs @ vars, env

let rec infer ~env level expr =
  match expr with
  | TTree.ELiteral value ->
      let value = infer_literal value in
      value
  | TTree.ELower value -> (
      match Environment.lookup_infer ~name:value env with
      | Some pretype -> pretype
      | None -> raise (TError (UnboundedVariable value)))
  | TTree.EAnnot (value, annot) ->
      let _, anno = instantiate_annot level ([], annot) in
      let infered = infer ~env level value in
      let () = subsume level anno infered in
      anno
  | TTree.ELet (name, bind, body) ->
      let bind = infer ~env (Level.next level) bind in
      infer ~env:(Environment.extend_infer ~name bind env) level body
  | TTree.ETuple values ->
      (* TODO: function to infer many *)
      let f expr = infer ~env level expr in
      let values = List.map f values in
      TTree.TTuple values
  | TTree.EApply (lambda, argm) ->
      let level = Level.next level in
      let infer_lambda = infer ~env level lambda in
      let insta_lambda = instantiate level infer_lambda in
      let param, return = destruct_lambda_type insta_lambda in
      let () = infer_arg ~env level param argm in
      let return = instantiate level return in
      let return = generalize (Level.prev level) return in
      return
  | TTree.ELambda (param, body) -> (
      let annot, variables, env = infer_patt ~env level param in
      let infer_return = infer ~env (Level.next level) body in
      let insta_return =
        match is_annotated body, infer_return with
        | true, _ -> infer_return
        | false, _ -> instantiate (Level.next level) infer_return
      in
      match List.for_all is_monomorphic variables with
      | true -> generalize level (TTree.TArrow (annot, insta_return))
      | false -> raise (TError ExpectMonotype))

and infer_arg ~env level param_ty arg_expr =
  let arg_ty = infer ~env level arg_expr in
  match is_annotated arg_expr with
  | true -> unify param_ty arg_ty
  | false -> subsume level param_ty arg_ty

let infer_term ~env term =
  match term with
  | TTree.TBind { name; annot = None; body } ->
      let level = Level.base in
      let body = infer ~env level body in
      Environment.extend_infer ~name body env
  | TTree.TBind { name; annot = Some annot; body } ->
      let level = Level.base in
      let _, annot = instantiate_annot level ([], annot) in
      let body = infer ~env level body in
      let () = subsume level annot body in
      Environment.extend_infer ~name annot env

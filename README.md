## wip

Initial code:

```ocaml
module Transl = struct
  module Context = struct
    type context = TTree.poly Ctx.IMap.t
    type t = context

    let empty = Ctx.IMap.empty
    let lookup ~name ctx = Ctx.IMap.add name ctx
    let extend ~name elt ctx = Ctx.IMap.add ctx name elt
  end

  let enter_skolem ~context ~entries name =
    let idid, bound = Skolem.make_bound () in
    let new_context = Env.extend ~name (Some bound) context in
    let new_entries = idid :: entries in
    (new_context, new_entries, bound)

  let rec transl_type_wrapper variables pretype level =
    let context = Hashtbl.create 16 in
    let entries = ref [] in
    List.iter (fun var_name -> Hashtbl.replace context var_name None) variables ;
    List.rev !entries, transl_type ~context ~entries ~level pretype

  (* ... *)
end
```

Desired abstraction:

I want to abstract:

- enter_skolem, transl_type_wrapper
  - don't use ref
  - split the functions into others to be more readable
  - turn transl_type_wrapper more readable, in the sense of reduce the overhead

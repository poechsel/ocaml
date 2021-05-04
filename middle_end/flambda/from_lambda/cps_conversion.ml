(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Use CPS".
   -- A. Kennedy, "Compiling with Continuations Continued", ICFP 2007.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module I = Ilambda
module L = Lambda
module C = Lambda_conversions

type proto_switch = {
  numconsts : int;
  consts : (int * L.lambda) list;
  failaction : L.lambda option;
}

type primitive_transform_result =
  | Primitive of L.primitive * L.lambda list * L.scoped_location
  | Transformed of L.lambda

let check_let_rec_bindings bindings =
  List.map (fun (binding : Lambda.lambda) ->
      match binding with
      | Lfunction func -> func
      | _ ->
        Misc.fatal_errorf "Only [Lfunction] expressions are permitted in \
            [Lletrec] bindings upon entry to CPS conversion: %a"
          Printlambda.lambda binding)
    bindings

let name_for_function (func : Lambda.lfunction) =
  (* Name anonymous functions by their source location, if known. *)
  match func.loc with
  | Loc_unknown -> "anon-fn"
  | Loc_known { loc; _ } ->
    Format.asprintf "anon-fn[%a]" Location.print_compact loc

(* CR-soon mshinwell: Remove mutable state. *)
let static_exn_env = ref Numbers.Int.Map.empty
let try_stack = ref []
let try_stack_at_handler = ref Continuation.Map.empty
let recursive_static_catches = ref Numbers.Int.Set.empty
let mutable_variables = ref Ident.Set.empty

let mark_as_recursive_static_catch cont =
  if Numbers.Int.Set.mem cont !recursive_static_catches then begin
    Misc.fatal_errorf "Static catch with continuation %d already marked as \
        recursive -- is it being redefined?"
      cont
  end;
  recursive_static_catches := Numbers.Int.Set.add cont !recursive_static_catches

let _print_stack ppf stack =
  Format.fprintf ppf "%a"
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
      (fun ppf (_id, cont) -> Format.fprintf ppf "%a" Continuation.print cont))
    stack

(* Uses of [Lstaticfail] that jump out of try-with handlers need special care:
   the correct number of pop trap operations must be inserted. *)
let compile_staticfail ~(continuation : Continuation.t) ~args =
  let try_stack_at_handler =
    match Continuation.Map.find continuation !try_stack_at_handler with
    | exception Not_found ->
      Misc.fatal_errorf "No try stack recorded for handler %a"
        Continuation.print continuation
    | stack -> stack
  in
  let try_stack_now = !try_stack in
  if List.length try_stack_at_handler > List.length try_stack_now then begin
    Misc.fatal_errorf "Cannot jump to continuation %a: it would involve \
        jumping into a try-with body"
      Continuation.print continuation
  end;
  assert (Continuation.Set.subset
    (Continuation.Set.of_list try_stack_at_handler)
    (Continuation.Set.of_list try_stack_now));
  let rec add_pop_traps ~try_stack_now ~try_stack_at_handler =
    let add_pop cont ~try_stack_now after_pop =
      let mk_remaining_traps =
        add_pop_traps ~try_stack_now ~try_stack_at_handler
      in
      let wrapper_cont = Continuation.create () in
      let trap_action : I.trap_action =
        Pop { exn_handler = cont; }
      in
      let body = I.Apply_cont (wrapper_cont, Some trap_action, []) in
      I.Let_cont {
        name = wrapper_cont;
        is_exn_handler = false;
        params = [];
        recursive = Nonrecursive;
        body;
        handler = mk_remaining_traps after_pop;
      }
    in
    let no_pop after_pop = after_pop in
    match try_stack_now, try_stack_at_handler with
    | [], [] -> no_pop
    | cont1 :: try_stack_now, cont2 :: _ ->
      if Continuation.equal cont1 cont2 then no_pop
      else add_pop cont1 ~try_stack_now
    | cont :: try_stack_now, [] -> add_pop cont ~try_stack_now
    | [], _ :: _ -> assert false  (* see above *)
  in
  let mk_poptraps =
    add_pop_traps ~try_stack_now ~try_stack_at_handler
  in
  mk_poptraps (I.Apply_cont (continuation, None, args))

let switch_for_if_then_else ~cond ~ifso ~ifnot =
  (* CR mshinwell: We need to make sure that [cond] is {0, 1}-valued.
     The frontend should have been fixed on this branch for this. *)
  let switch : Lambda.lambda_switch =
    { sw_numconsts = 2;
      sw_consts = [0, ifnot; 1, ifso];
      sw_numblocks = 0;
      sw_blocks = [];
      sw_failaction = None;
      sw_tags_to_sizes = Tag.Scannable.Map.empty;
    }
  in
  L.Lswitch (cond, switch, Loc_unknown)

let transform_primitive (prim : L.primitive) args loc =
  match prim, args with
  | Psequor, [arg1; arg2] ->
    let const_true = Ident.create_local "const_true" in
    let cond = Ident.create_local "cond_sequor" in
    Transformed
      (L.Llet (Strict, Pgenval, const_true, Lconst (Const_base (Const_int 1)),
        (L.Llet (Strict, Pgenval, cond, arg1,
          switch_for_if_then_else
            ~cond:(L.Lvar cond)
            ~ifso:(L.Lvar const_true)
            ~ifnot:arg2))))
  | Psequand, [arg1; arg2] ->
    let const_false = Ident.create_local "const_false" in
    let cond = Ident.create_local "cond_sequand" in
    Transformed
      (L.Llet (Strict, Pgenval, const_false, Lconst (Const_base (Const_int 0)),
        (L.Llet (Strict, Pgenval, cond, arg1,
          switch_for_if_then_else
            ~cond:(L.Lvar cond)
            ~ifso:arg2
            ~ifnot:(L.Lvar const_false)))))
  | (Psequand | Psequor), _ ->
    Misc.fatal_error "Psequand / Psequor must have exactly two arguments"
  (* Removed. Should be safe, but will no longer catch misuses.
     CR keryan: do we still need this primitive ?
  | Pflambda_isint, _ ->
    Misc.fatal_error "[Pflambda_isint] should not exist at this stage" *)
  | Pisint, [arg] ->
    Transformed
      (switch_for_if_then_else
        ~cond:(L.Lprim (Pflambda_isint, [arg], loc))
        ~ifso:(L.Lconst (Const_base (Const_int 1)))
        ~ifnot:(L.Lconst (Const_base (Const_int 0))))
  | (Pidentity | Pbytes_to_string | Pbytes_of_string), [arg] -> Transformed arg
  | Pignore, [arg] ->
    let ident = Ident.create_local "ignore" in
    let result = L.Lconst (Const_base (Const_int 0)) in
    Transformed (L.Llet (Strict, Pgenval, ident, arg, result))
  | Pdirapply, [funct; arg]
  | Prevapply, [arg; funct] ->
    let apply : L.lambda_apply =
      { ap_func = funct;
        ap_args = [arg];
        ap_loc = loc;
        ap_should_be_tailcall = false;
        (* CR-someday lwhite: it would be nice to be able to give
           inlined attributes to functions applied with the application
           operators. *)
        ap_inlined = Default_inline;
        ap_specialised = Default_specialise;
      }
    in
    Transformed (L.Lapply apply)
  | Psetfield (_, _, _), [L.Lprim (Pgetglobal _, [], _); _] ->
    Misc.fatal_error "[Psetfield (Pgetglobal ...)] is \
      forbidden upon entry to the middle end"
  | Pfield ({ index; _ }, _), _ when index < 0 ->
    Misc.fatal_error "Pfield with negative field index"
  | Pfloatfield (i, _), _ when i < 0 ->
    Misc.fatal_error "Pfloatfield with negative field index"
  | Psetfield ({ index; _ }, _, _), _ when index < 0 ->
    Misc.fatal_error "Psetfield with negative field index"
  | Pmakeblock (tag, _, _), _
      when tag < 0 || tag >= Obj.no_scan_tag ->
    Misc.fatal_errorf "Pmakeblock with wrong or non-scannable block tag %d" tag
  | Pmakefloatblock _mut, args when List.length args < 1 ->
    Misc.fatal_errorf "Pmakefloatblock must have at least one argument"
  | Pfloatcomp CFnlt, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFlt, args, loc)], loc)
  | Pfloatcomp CFngt, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFgt, args, loc)], loc)
  | Pfloatcomp CFnle, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFle, args, loc)], loc)
  | Pfloatcomp CFnge, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFge, args, loc)], loc)
  | Pbigarrayref (_unsafe, num_dimensions, kind, layout), args ->
    begin match C.convert_bigarray_kind kind,
                C.convert_bigarray_layout layout with
    | Some _, Some _ ->
      Primitive (prim, args, loc)
    | None, None | None, Some _ | Some _, None ->
      if 1 <= num_dimensions && num_dimensions <= 3 then begin
        let arity = 1 + num_dimensions in
        let name = "caml_ba_get_" ^ string_of_int num_dimensions in
        let desc = Primitive.simple ~name ~arity ~alloc:true in
        Primitive (L.Pccall desc, args, loc)
      end else begin
        Misc.fatal_errorf
          "Cps_conversion.transform_primitive: \
           Pbigarrayref with unknown layout and elements should only have \
           dimensions between 1 and 3 (see translprim)."
      end
    end
  | Pbigarrayset (_unsafe, num_dimensions, kind, layout), args ->
    begin match C.convert_bigarray_kind kind,
                C.convert_bigarray_layout layout with
    | Some _, Some _ ->
      Primitive (prim, args, loc)
    | None, None | None, Some _ | Some _, None ->
      if 1 <= num_dimensions && num_dimensions <= 3 then begin
        let arity = 2 + num_dimensions in
        let name = "caml_ba_set_" ^ string_of_int num_dimensions in
        let desc = Primitive.simple ~name ~arity ~alloc:true in
        Primitive (L.Pccall desc, args, loc)
      end else begin
        Misc.fatal_errorf
          "Cps_conversion.transform_primimive: \
           Pbigarrayset with unknown layout and elements should only have \
           dimensions between 1 and 3 (see translprim)."
      end
    end
  | _, _ -> Primitive (prim, args, loc)

let rec_catch_for_while_loop cond body =
  let cont = L.next_raise_count () in
  mark_as_recursive_static_catch cont;
  let cond_result = Ident.create_local "while_cond_result" in
  let lam : L.lambda =
    Lstaticcatch (
      Lstaticraise (cont, []),
      (cont, []),
      Llet (Strict, Pgenval, cond_result, cond,
        Lifthenelse (Lvar cond_result,
          Lsequence (
            body,
            Lstaticraise (cont, [])),
          Lconst (Const_base (Const_int 0)))))
  in lam

let rec_catch_for_for_loop
      ident start stop (dir : Asttypes.direction_flag) body =
  let cont = L.next_raise_count () in
  mark_as_recursive_static_catch cont;
  let start_ident = Ident.create_local "for_start" in
  let stop_ident = Ident.create_local "for_stop" in
  let first_test : L.lambda =
    match dir with
    | Upto ->
       Lprim (Pintcomp Cle,
         [L.Lvar start_ident; L.Lvar stop_ident],
         Loc_unknown)
    | Downto ->
       Lprim (Pintcomp Cge,
         [L.Lvar start_ident; L.Lvar stop_ident],
         Loc_unknown)
  in
  let subsequent_test : L.lambda =
    Lprim (Pintcomp Cne, [L.Lvar ident; L.Lvar stop_ident], Loc_unknown)
  in
  let one : L.lambda = Lconst (Const_base (Const_int 1)) in
  let next_value_of_counter =
    match dir with
    | Upto -> L.Lprim (Paddint, [L.Lvar ident; one], Loc_unknown)
    | Downto -> L.Lprim (Psubint, [L.Lvar ident; one], Loc_unknown)
  in
  let lam : L.lambda =
    (* Care needs to be taken here not to cause overflow if, for an
       incrementing for-loop, the upper bound is [max_int]; likewise, for
       a decrementing for-loop, if the lower bound is [min_int]. *)
    Llet (Strict, Pgenval, start_ident, start,
      Llet (Strict, Pgenval, stop_ident, stop,
        Lifthenelse (first_test,
          Lstaticcatch (
            Lstaticraise (cont, [L.Lvar start_ident]),
            (cont, [ident, Pgenval]),
            Lsequence (
              body,
              Lifthenelse (subsequent_test,
                Lstaticraise (cont, [next_value_of_counter]),
                L.lambda_unit))),
          L.lambda_unit)))
  in lam

let rec cps_non_tail (lam : L.lambda) (k : Ident.t -> Ilambda.t)
          (k_exn : Continuation.t) : Ilambda.t =
  match lam with
  | Lvar id ->
    if Ident.Set.mem id !mutable_variables then
      name_then_cps_non_tail "mutable_read" (I.Mutable_read id) k k_exn
    else
      k id
  | Lconst const ->
    name_then_cps_non_tail "const" (I.Simple (Const const)) k k_exn
  | Lapply { ap_func; ap_args; ap_loc; ap_should_be_tailcall; ap_inlined;
      ap_specialised; } ->
    cps_non_tail_list ap_args (fun args ->
      cps_non_tail ap_func (fun func ->
        let continuation = Continuation.create () in
        let result_var = Ident.create_local "apply_result" in
        let after = k result_var in
        let exn_continuation : I.exn_continuation =
          { exn_handler = k_exn;
            extra_args = [];
          }
        in
        let apply : Ilambda.apply = {
          kind = Function;
          func;
          continuation;
          exn_continuation;
          args;
          loc = ap_loc;
          should_be_tailcall = ap_should_be_tailcall;
          inlined = ap_inlined;
          specialised = ap_specialised;
        } in
        I.Let_cont {
          name = continuation;
          is_exn_handler = false;
          params = [result_var, I.Not_user_visible, Pgenval];
          recursive = Nonrecursive;
          body = Apply apply;
          handler = after;
        }) k_exn)
      k_exn
  | Lfunction func ->
    let id = Ident.create_local (name_for_function func) in
    let func = cps_function func in
    let body = k id in
    Let_rec ([id, func], body)
  | Llet (Variable, value_kind, id, defining_expr, body) ->
    mutable_variables := Ident.Set.add id !mutable_variables;
    let temp_id = Ident.create_local "let_mutable" in
    let body = cps_non_tail body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr = cps_tail defining_expr after_defining_expr k_exn in
    let let_mutable : I.let_mutable =
      { id;
        initial_value = Var temp_id;
        contents_kind = value_kind;
        body;
      }
    in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [temp_id, I.Not_user_visible, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = Let_mutable let_mutable;
    }
  | Llet (Alias, Pgenval, id, Lfunction func, body) ->
    (* This case is here to get function names right. *)
    let func = cps_function func in
    let body = cps_non_tail body k k_exn in
    Let_rec ([id, func], body)
  | Llet (_, value_kind, id, Lconst const, body) ->
    (* This case avoids extraneous continuations. *)
    let body = cps_non_tail body k k_exn in
    I.Let (id, User_visible, value_kind, Simple (Const const), body)
  | Llet (let_kind, value_kind, id, Lprim (prim, args, loc), body) ->
    begin match transform_primitive prim args loc with
    | Primitive (prim, args, loc) ->
      (* This case avoids extraneous continuations. *)
      let exn_continuation : I.exn_continuation option =
        if L.primitive_can_raise prim then
          Some {
            exn_handler = k_exn;
            extra_args = [];
          }
        else None
      in
      cps_non_tail_list args (fun args ->
          let body = cps_non_tail body k k_exn in
          I.Let (id, User_visible, value_kind,
            Prim { prim; args; loc; exn_continuation; },
            body))
        k_exn
    | Transformed lam ->
      cps_non_tail (L.Llet (let_kind, value_kind, id, lam, body)) k k_exn
    end
  | Llet (_let_kind, value_kind, id, defining_expr, body) ->
    let body = cps_non_tail body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr = cps_tail defining_expr after_defining_expr k_exn in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [id, I.User_visible, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = body;
    }
  | Lletrec (bindings, body) ->
    let idents, bindings = List.split bindings in
    let bindings = List.map cps_function (check_let_rec_bindings bindings) in
    let body = cps_non_tail body k k_exn in
    Let_rec (List.combine idents bindings, body)
  | Lprim (prim, args, loc) ->
    begin match transform_primitive prim args loc with
    | Primitive (prim, args, loc) ->
      let name = Printlambda.name_of_primitive prim in
      let result_var = Ident.create_local name in
      let exn_continuation : I.exn_continuation option =
        if L.primitive_can_raise prim then
          Some {
            exn_handler = k_exn;
            extra_args = [];
          }
        else None
      in
      cps_non_tail_list args (fun args ->
          I.Let (result_var,
            Not_user_visible,
            Pgenval,
            Prim { prim; args; loc; exn_continuation; },
            k result_var))
        k_exn
    | Transformed lam -> cps_non_tail lam k k_exn
    end
  | Lswitch (scrutinee,
      { sw_numconsts; sw_consts; sw_numblocks = _; sw_blocks; sw_failaction;
        sw_tags_to_sizes = _; }, _loc) ->
    begin match sw_blocks with
    | [] -> ()
    | _ -> Misc.fatal_error "Lswitch `block' cases are forbidden"
    end;
    let after_switch = Continuation.create () in
    let result_var = Ident.create_local "switch_result" in
    let after = k result_var in
    let proto_switch : proto_switch =
      { numconsts = sw_numconsts;
        consts = sw_consts;
        failaction = sw_failaction;
      }
    in
    let body = cps_switch proto_switch ~scrutinee after_switch k_exn in
    Let_cont {
      name = after_switch;
      is_exn_handler = false;
      params = [result_var, I.Not_user_visible, Pgenval];
      recursive = Nonrecursive;
      body;
      handler = after;
    }
  | Lstringswitch _ ->
    Misc.fatal_error "Lstringswitch must be expanded prior to CPS conversion"
  | Lstaticraise (static_exn, args) ->
    let continuation =
      match Numbers.Int.Map.find static_exn !static_exn_env with
      | exception Not_found ->
        Misc.fatal_errorf "Unbound static exception %d" static_exn
      | continuation -> continuation
    in
    cps_non_tail_list args
      (fun args -> compile_staticfail ~continuation ~args) k_exn
  | Lstaticcatch (body, (static_exn, args), handler) ->
    let continuation = Continuation.create () in
    static_exn_env := Numbers.Int.Map.add static_exn continuation
      !static_exn_env;
    try_stack_at_handler := Continuation.Map.add continuation !try_stack
      !try_stack_at_handler;
    let after_continuation = Continuation.create () in
    let result_var = Ident.create_local "staticcatch_result" in
    let body = cps_tail body after_continuation k_exn in
    let handler = cps_tail handler after_continuation k_exn in
    let recursive : Asttypes.rec_flag =
      if Numbers.Int.Set.mem static_exn !recursive_static_catches then
        Recursive
      else
        Nonrecursive
    in
    Let_cont {
      name = after_continuation;
      is_exn_handler = false;
      params = [result_var, I.Not_user_visible, Pgenval];
      recursive = Nonrecursive;
      body =
        Let_cont {
          name = continuation;
          is_exn_handler = false;
          params = List.map (fun (arg, kind) -> arg, I.User_visible, kind) args;
          recursive;
          body;
          handler;
        };
      handler = k result_var;
    };
  | Lsend (meth_kind, meth, obj, args, loc) ->
    cps_non_tail_simple obj (fun obj ->
      cps_non_tail meth (fun meth ->
        cps_non_tail_list args (fun args ->
          let continuation = Continuation.create () in
          let result_var = Ident.create_local "send_result" in
          let after = k result_var in
          let exn_continuation : I.exn_continuation =
            { exn_handler = k_exn;
              extra_args = [];
            }
          in
          let apply : Ilambda.apply = {
            kind = Method { kind = meth_kind; obj; };
            func = meth;
            continuation;
            exn_continuation;
            args;
            loc;
            should_be_tailcall = false;
            inlined = Default_inline;
            specialised = Default_specialise;
          } in
          I.Let_cont {
            name = continuation;
            is_exn_handler = false;
            params = [result_var, Not_user_visible, Pgenval];
            recursive = Nonrecursive;
            body = Apply apply;
            handler = after;
          }) k_exn) k_exn) k_exn
  | Ltrywith (body, id, handler) ->
    let body_result = Ident.create_local "body_result" in
    let result_var = Ident.create_local "try_with_result" in
    let body_continuation = Continuation.create () in
    let handler_continuation = Continuation.create ~sort:Exn () in
    let poptrap_continuation = Continuation.create () in
    let after_continuation = Continuation.create () in
    let old_try_stack = !try_stack in
    try_stack := handler_continuation :: old_try_stack;
    let body =
      cps_tail body poptrap_continuation handler_continuation
    in
    try_stack := old_try_stack;
    let handler = cps_tail handler after_continuation k_exn in
    Let_cont {
      name = after_continuation;
      is_exn_handler = false;
      params = [result_var, Not_user_visible, Pgenval];
      recursive = Nonrecursive;
      body =
        Let_cont {
          name = handler_continuation;
          is_exn_handler = true;
          params = [id, User_visible, Pgenval];
          recursive = Nonrecursive;
          body =
            Let_cont {
              name = poptrap_continuation;
              is_exn_handler = false;
              params = [body_result, Not_user_visible, Pgenval];
              recursive = Nonrecursive;
              body =
                Let_cont {
                  name = body_continuation;
                  is_exn_handler = false;
                  params = [];
                  recursive = Nonrecursive;
                  body =
                    Apply_cont (body_continuation,
                      Some (I.Push {
                        exn_handler = handler_continuation;
                      }),
                      []);
                  handler = body;
                };
              handler = Apply_cont (after_continuation,
                Some (I.Pop { exn_handler = handler_continuation; }),
                [Ilambda.Var body_result]);
            };
          handler;
        };
      handler = k result_var;
    }
  | Lifthenelse (cond, ifso, ifnot) ->
    let lam = switch_for_if_then_else ~cond ~ifso ~ifnot in
    cps_non_tail lam k k_exn
  | Lsequence (lam1, lam2) ->
    let ident = Ident.create_local "sequence" in
    cps_non_tail (L.Llet (Strict, Pgenval, ident, lam1, lam2)) k k_exn
  | Lwhile (cond, body) ->
    let loop = rec_catch_for_while_loop cond body in
    cps_non_tail loop k k_exn
  | Lfor (ident, start, stop, dir, body) ->
    let loop = rec_catch_for_for_loop ident start stop dir body in
    cps_non_tail loop k k_exn
  | Lassign (being_assigned, new_value) ->
    cps_non_tail_simple new_value (fun new_value ->
        name_then_cps_non_tail "assign"
          (I.Assign { being_assigned; new_value; })
          k k_exn)
      k_exn
  | Lifused _ | Levent _ ->
    Misc.fatal_errorf "Term should have been eliminated by [Prepare_lambda]: %a"
      Printlambda.lambda lam

and cps_non_tail_simple (lam : L.lambda) (k : Ilambda.simple -> Ilambda.t)
      (k_exn : Continuation.t) : Ilambda.t =
  match lam with
  | Lvar id when not (Ident.Set.mem id !mutable_variables) -> k (Ilambda.Var id)
  | Lconst const -> k (Ilambda.Const const)
  | Lvar _ (* mutable read *)
  | Lapply _
  | Lfunction _
  | Llet _
  | Lletrec _
  | Lprim _
  | Lswitch _
  | Lstringswitch _
  | Lstaticraise _
  | Lstaticcatch _
  | Ltrywith _
  | Lifthenelse _
  | Lsequence _
  | Lwhile _
  | Lfor _
  | Lassign _
  | Lsend _
  | Levent _
  | Lifused _ -> cps_non_tail lam (fun id -> k (Ilambda.Var id)) k_exn

and cps_tail (lam : L.lambda) (k : Continuation.t) (k_exn : Continuation.t)
      : Ilambda.t =
  match lam with
  | Lvar id ->
    if Ident.Set.mem id !mutable_variables then
      name_then_cps_tail "mutable_read" (I.Mutable_read id) k k_exn
    else
      Apply_cont (k, None, [Ilambda.Var id])
  | Lconst const ->
    name_then_cps_tail "const" (I.Simple (Const const)) k k_exn
  | Lapply apply ->
    cps_non_tail_list apply.ap_args (fun args ->
      cps_non_tail apply.ap_func (fun func ->
        let exn_continuation : I.exn_continuation =
          { exn_handler = k_exn;
            extra_args = [];
          }
        in
        let apply : I.apply = {
          kind = Function;
          func;
          continuation = k;
          exn_continuation;
          args;
          loc = apply.ap_loc;
          should_be_tailcall = apply.ap_should_be_tailcall;
          inlined = apply.ap_inlined;
          specialised = apply.ap_specialised;
        } in
        I.Apply apply) k_exn) k_exn
  | Lfunction func ->
    let id = Ident.create_local (name_for_function func) in
    let func = cps_function func in
    Let_rec ([id, func], Apply_cont (k, None, [Ilambda.Var id]))
  | Llet (Variable, value_kind, id, defining_expr, body) ->
    mutable_variables := Ident.Set.add id !mutable_variables;
    let temp_id = Ident.create_local "let_mutable" in
    let body = cps_tail body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr =
      cps_tail defining_expr after_defining_expr k_exn
    in
    let let_mutable : I.let_mutable =
      { id;
        initial_value = I.Var temp_id;
        contents_kind = value_kind;
        body;
      }
    in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [temp_id, Not_user_visible, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = Let_mutable let_mutable;
    }
  | Llet (Alias, Pgenval, id, Lfunction func, body) ->
    (* CR mshinwell: Why is this case restricted to [Alias]? *)
    (* This case is here to get function names right. *)
    let func = cps_function func in
    let body = cps_tail body k k_exn in
    Let_rec ([id, func], body)
  | Llet (_, value_kind, id, Lconst const, body) ->
    (* This case avoids extraneous continuations. *)
    let body = cps_tail body k k_exn in
    I.Let (id, User_visible, value_kind, Simple (Const const), body)
  | Llet (let_kind, value_kind, id, Lprim (prim, args, loc), body) ->
    begin match transform_primitive prim args loc with
    | Primitive (prim, args, loc) ->
      (* This case avoids extraneous continuations. *)
      let exn_continuation : I.exn_continuation option =
        if L.primitive_can_raise prim then
          Some {
            exn_handler = k_exn;
            extra_args = [];
          }
        else None
      in
      cps_non_tail_list args (fun args ->
          let body = cps_tail body k k_exn in
          I.Let (id, User_visible, value_kind,
            Prim { prim; args; loc; exn_continuation; },
            body))
        k_exn
    | Transformed lam ->
       cps_tail (L.Llet (let_kind, value_kind, id, lam, body)) k k_exn
    end
  | Llet (_let_kind, _value_kind, id, Lassign (being_assigned, new_value),
      body) ->
    (* This case is also to avoid extraneous continuations in code that
       relies on the ref-conversion optimisation. *)
    cps_non_tail_simple new_value (fun new_value ->
        let body = cps_tail body k k_exn in
        I.Let (id, User_visible, Pgenval,
          I.Assign { being_assigned; new_value; },
          body))
      k_exn
  | Llet (_let_kind, value_kind, id, defining_expr, body) ->
    let body = cps_tail body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr = cps_tail defining_expr after_defining_expr k_exn in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [id, User_visible, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = body;
    }
  | Lletrec (bindings, body) ->
    let idents, bindings = List.split bindings in
    let bindings = List.map cps_function (check_let_rec_bindings bindings) in
    let body = cps_tail body k k_exn in
    Let_rec (List.combine idents bindings, body)
  | Lprim (prim, args, loc) ->
    begin match transform_primitive prim args loc with
    | Primitive (prim, args, loc) ->
      (* CR mshinwell: Arrange for "args" to be named. *)
      let name = Printlambda.name_of_primitive prim in
      let result_var = Ident.create_local name in
      let exn_continuation : I.exn_continuation option =
        if L.primitive_can_raise prim then
          Some {
            exn_handler = k_exn;
            extra_args = [];
          }
        else None
      in
      cps_non_tail_list args (fun args ->
          I.Let (result_var, Not_user_visible, Pgenval,
            Prim { prim; args; loc; exn_continuation; },
            Apply_cont (k, None, [Ilambda.Var result_var]))) k_exn
    | Transformed lam -> cps_tail lam k k_exn
    end
  | Lswitch (scrutinee,
      { sw_numconsts; sw_consts; sw_numblocks = _; sw_blocks; sw_failaction;
        sw_tags_to_sizes = _; }, _loc) ->
    begin match sw_blocks with
    | [] -> ()
    | _ -> Misc.fatal_error "Lswitch `block' cases are forbidden"
    end;
    let proto_switch : proto_switch =
      { numconsts = sw_numconsts;
        consts = sw_consts;
        failaction = sw_failaction;
      }
    in
    cps_switch proto_switch ~scrutinee k k_exn
  | Lstringswitch _ ->
    Misc.fatal_error "Lstringswitch must be expanded prior to CPS conversion"
  | Lstaticraise (static_exn, args) ->
    let continuation =
      match Numbers.Int.Map.find static_exn !static_exn_env with
      | exception Not_found ->
        Misc.fatal_errorf "Unbound static exception %d" static_exn
      | continuation -> continuation
    in
    cps_non_tail_list args
      (fun args -> compile_staticfail ~continuation ~args) k_exn
  | Lstaticcatch (body, (static_exn, args), handler) ->
    let continuation = Continuation.create () in
    static_exn_env := Numbers.Int.Map.add static_exn continuation
      !static_exn_env;
    try_stack_at_handler := Continuation.Map.add continuation !try_stack
      !try_stack_at_handler;
    let body = cps_tail body k k_exn in
    let handler = cps_tail handler k k_exn in
    let recursive : Asttypes.rec_flag =
      if Numbers.Int.Set.mem static_exn !recursive_static_catches then
        Recursive
      else
        Nonrecursive
    in
    Let_cont {
      name = continuation;
      is_exn_handler = false;
      params = List.map (fun (arg, kind) -> arg, I.User_visible, kind) args;
      recursive;
      body;
      handler;
    }
  | Lsend (meth_kind, meth, obj, args, loc) ->
    cps_non_tail_simple obj (fun obj ->
      cps_non_tail meth (fun meth ->
        cps_non_tail_list args (fun args ->
          let exn_continuation : I.exn_continuation =
            { exn_handler = k_exn;
              extra_args = [];
            }
          in
          let apply : Ilambda.apply = {
            kind = Method { kind = meth_kind; obj; };
            func = meth;
            continuation = k;
            exn_continuation;
            args;
            loc;
            should_be_tailcall = false;
            inlined = Default_inline;
            specialised = Default_specialise;
          } in
          I.Apply apply) k_exn) k_exn) k_exn
  | Lassign (being_assigned, new_value) ->
    cps_non_tail_simple new_value (fun new_value ->
        name_then_cps_tail "assign" (I.Assign { being_assigned; new_value; })
        k k_exn)
      k_exn
  | Ltrywith (body, id, handler) ->
    let body_result = Ident.create_local "body_result" in
    let body_continuation = Continuation.create () in
    let handler_continuation = Continuation.create ~sort:Exn () in
    let poptrap_continuation = Continuation.create () in
    let old_try_stack = !try_stack in
    try_stack := handler_continuation :: old_try_stack;
    let body = cps_tail body poptrap_continuation handler_continuation in
    try_stack := old_try_stack;
    let handler = cps_tail handler k k_exn in
    Let_cont {
      name = handler_continuation;
      is_exn_handler = true;
      params = [id, User_visible, Pgenval];
      recursive = Nonrecursive;
      body =
        Let_cont {
          name = poptrap_continuation;
          is_exn_handler = false;
          params = [body_result, Not_user_visible, Pgenval];
          recursive = Nonrecursive;
          body =
            Let_cont {
              name = body_continuation;
              is_exn_handler = false;
              params = [];
              recursive = Nonrecursive;
              body =
                Apply_cont (body_continuation,
                  Some (I.Push { exn_handler = handler_continuation; }),
                  []);
              handler = body;
            };
          handler = Apply_cont (k, Some (
            I.Pop { exn_handler = handler_continuation; }),
            [Ilambda.Var body_result]);
        };
      handler;
    }
  | Lifthenelse (cond, ifso, ifnot) ->
    let lam = switch_for_if_then_else ~cond ~ifso ~ifnot in
    cps_tail lam k k_exn
  | Lsequence (lam1, lam2) ->
    let ident = Ident.create_local "sequence" in
    cps_tail (L.Llet (Strict, Pgenval, ident, lam1, lam2)) k k_exn
  | Lwhile (cond, body) ->
    let loop = rec_catch_for_while_loop cond body in
    cps_tail loop k k_exn
  | Lfor (ident, start, stop, dir, body) ->
    let loop = rec_catch_for_for_loop ident start stop dir body in
    cps_tail loop k k_exn
  | Lifused _ | Levent _ ->
    Misc.fatal_errorf "Term should have been eliminated by [Prepare_lambda]: %a"
      Printlambda.lambda lam

and name_then_cps_non_tail name defining_expr k _k_exn : I.t =
  let id = Ident.create_local name in
  let body = k id in
  Let (id, Not_user_visible, Pgenval, defining_expr, body)

and name_then_cps_tail name defining_expr k _k_exn : I.t =
  let id = Ident.create_local name in
  Let (id, Not_user_visible, Pgenval, defining_expr,
    Apply_cont (k, None, [Ilambda.Var id]))

and cps_non_tail_list lams k k_exn =
  let lams = List.rev lams in  (* Always evaluate right-to-left. *)
  cps_non_tail_list_core lams (fun ids -> k (List.rev ids)) k_exn

and cps_non_tail_list_core (lams : L.lambda list)
      (k : Ilambda.simple list -> Ilambda.t)
      (k_exn : Continuation.t) =
  match lams with
  | [] -> k []
  | lam::lams ->
    cps_non_tail_simple lam (fun simple ->
      cps_non_tail_list_core lams (fun simples -> k (simple :: simples)) k_exn)
      k_exn

and cps_function ({ kind; params; return; body; attr; loc; } : L.lfunction)
      : Ilambda.function_declaration =
  let body_cont = Continuation.create ~sort:Return () in
  let body_exn_cont = Continuation.create ~sort:Exn () in
  let free_idents_of_body = Lambda.free_variables body in
  let body = cps_tail body body_cont body_exn_cont in
  let exn_continuation : I.exn_continuation =
    { exn_handler = body_exn_cont;
      extra_args = [];
    }
  in
  { kind = kind;
    return_continuation = body_cont;
    exn_continuation;
    params = params;
    return;
    body;
    free_idents_of_body;
    attr = attr;
    loc = loc;
    stub = false;
  }

and cps_switch (switch : proto_switch) ~scrutinee (k : Continuation.t)
      (k_exn : Continuation.t) : Ilambda.t =
  let consts_rev, wrappers =
    List.fold_left (fun (consts_rev, wrappers) (arm, (action : L.lambda)) ->
        match action with
        | Lvar var when not (Ident.Set.mem var !mutable_variables) ->
          let consts_rev = (arm, k, None, [Ilambda.Var var]) :: consts_rev in
          consts_rev, wrappers
        | Lconst cst ->
          let consts_rev = (arm, k, None, [Ilambda.Const cst]) :: consts_rev in
          consts_rev, wrappers
        | Lvar _ (* mutable *)
        | Lapply _
        | Lfunction _
        | Llet _
        | Lletrec _
        | Lprim _
        | Lswitch _
        | Lstringswitch _
        | Lstaticraise _
        | Lstaticcatch _
        | Ltrywith _
        | Lifthenelse _
        | Lsequence _
        | Lwhile _
        | Lfor _
        | Lassign _
        | Lsend _
        | Levent _
        | Lifused _ ->
          let action = cps_tail action k k_exn in
          match action with
          | Apply_cont (cont, trap, args) ->
            let consts_rev = (arm, cont, trap, args) :: consts_rev in
            consts_rev, wrappers
          | Let _ | Let_mutable _ | Let_rec _ | Let_cont _ | Apply _
          | Switch _ ->
            let cont = Continuation.create () in
            let consts_rev = (arm, cont, None, []) :: consts_rev in
            let wrappers = (cont, action) :: wrappers in
            consts_rev, wrappers)
      ([], [])
      switch.consts
  in
  let consts = List.rev consts_rev in
  let failaction, wrappers =
    match switch.failaction with
    | None -> None, wrappers
    | Some action ->
      let cont = Continuation.create () in
      let action = cps_tail action k k_exn in
      let wrappers = (cont, action) :: wrappers in
      Some (cont, None, []), wrappers
  in
  let switch : I.switch =
    { numconsts = switch.numconsts;
      consts;
      failaction;
    }
  in
  cps_non_tail scrutinee (fun scrutinee ->
      List.fold_left (fun body (cont, action) ->
          I.Let_cont {
            name = cont;
            is_exn_handler = false;
            params = [];
            recursive = Nonrecursive;
            body;
            handler = action;
          })
        (I.Switch (scrutinee, switch))
        wrappers)
    k_exn

let lambda_to_ilambda lam : Ilambda.program =
  static_exn_env := Numbers.Int.Map.empty;
  try_stack := [];
  try_stack_at_handler := Continuation.Map.empty;
  recursive_static_catches := Numbers.Int.Set.empty;
  mutable_variables := Ident.Set.empty;
  let the_end = Continuation.create ~sort:Define_root_symbol () in
  let the_end_exn = Continuation.create ~sort:Exn () in
  let ilam = cps_tail lam the_end the_end_exn in
  let exn_continuation : I.exn_continuation =
    { exn_handler = the_end_exn;
      extra_args = [];
    }
  in
  { expr = ilam;
    return_continuation = the_end;
    exn_continuation;
    uses_mutable_variables = not (Ident.Set.is_empty !mutable_variables);
  }

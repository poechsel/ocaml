(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42-66"]

open! Int_replace_polymorphic_compare
open! Flambda

module Acc = Closure_conversion_aux.Acc
module Env = Closure_conversion_aux.Env
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc
module Let_cont_with_acc = Closure_conversion_aux.Let_cont_with_acc
module Let_with_acc = Closure_conversion_aux.Let_with_acc
module Continuation_handler_with_acc =
  Closure_conversion_aux.Continuation_handler_with_acc

module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl

module K = Flambda_kind
module LC = Lambda_conversions
module P = Flambda_primitive
module VB = Var_in_binding_pos

(* Do not use [Simple.symbol], use this function instead, to ensure that
   we correctly compute the free names of [Code]. *)
let use_of_symbol_as_simple acc symbol =
  let acc = Acc.add_symbol_to_free_names ~symbol acc in
  acc, Simple.symbol symbol

let symbol_for_ident acc env id =
  let symbol = Env.symbol_for_global' env id in
  use_of_symbol_as_simple acc symbol

let register_const0 acc constant name =
  match Static_const.Map.find constant (Acc.shareable_constants acc) with
  | exception Not_found ->
    (* Create a variable to ensure uniqueness of the symbol. *)
    let var = Variable.create name in
    let symbol =
      Symbol.create (Compilation_unit.get_current_exn ())
        (Linkage_name.create
           (Variable.unique_name (Variable.rename var)))
    in
    let acc = Acc.add_declared_symbol ~symbol ~constant acc in
    let acc =
      if Static_const.can_share constant then
        Acc.add_shareable_constant ~symbol ~constant acc
      else acc
    in
    acc, symbol
  | symbol -> acc, symbol

let register_const acc constant name
  : Acc.t * Static_const.Field_of_block.t * string =
  let acc, symbol = register_const0 acc constant name in
  acc, Symbol symbol, name

let register_const_string acc str =
  register_const0 acc (Static_const.Immutable_string str) "string"

let rec declare_const acc (const : Lambda.structured_constant)
      : Acc.t * Static_const.Field_of_block.t * string =
  match const with
  | Const_base (Const_int c) ->
    acc, Tagged_immediate (Target_imm.int (Targetint.OCaml.of_int c)), "int"
  | Const_pointer p ->
    (* CR mshinwell: This needs to be removed. *)
    acc,
    Tagged_immediate (Target_imm.int (Targetint.OCaml.of_int p)),
    "const_ptr"
  | Const_base (Const_char c) ->
    acc, Tagged_immediate (Target_imm.char c), "char"
  | Const_base (Const_string (s, _, _)) ->
    let const, name =
      if Config.safe_string then
        Static_const.Immutable_string s, "immstring"
      else
        Static_const.Mutable_string { initial_value = s; }, "string"
    in
    register_const acc const name
  | Const_base (Const_float c) ->
    let c = Numbers.Float_by_bit_pattern.create (float_of_string c) in
    register_const acc (Boxed_float (Const c)) "float"
  | Const_base (Const_int32 c) ->
    register_const acc (Boxed_int32 (Const c)) "int32"
  | Const_base (Const_int64 c) ->
    register_const acc (Boxed_int64 (Const c)) "int64"
  | Const_base (Const_nativeint c) ->
    (* CR pchambart: this should be pushed further to lambda *)
    let c = Targetint.of_int64 (Int64.of_nativeint c) in
    register_const acc (Boxed_nativeint (Const c)) "nativeint"
  | Const_immstring c ->
    register_const acc (Immutable_string c) "immstring"
  | Const_float_block c ->
    register_const acc
      (Immutable_float_block
         (List.map (fun s ->
           let f = Numbers.Float_by_bit_pattern.create (float_of_string s) in
           Or_variable.Const f) c))
      "float_block"
  | Const_float_array c ->
    register_const acc
      (Immutable_float_array
         (List.map (fun s ->
           let f = Numbers.Float_by_bit_pattern.create (float_of_string s) in
           Or_variable.Const f) c))
      "float_array"
  | Const_block (tag, consts) ->
    let acc, field_of_blocks =
      List.fold_left_map
        (fun acc c ->
           let acc, f, _ = declare_const acc c in
           acc, f)
        acc
        consts
    in
    let const : Static_const.t  =
      Block (Tag.Scannable.create_exn tag, Immutable, field_of_blocks)
    in
    register_const acc const "const_block"

let close_const0 acc (const : Lambda.structured_constant) =
  let acc, const, name = declare_const acc const in
  match const with
  | Tagged_immediate c ->
    acc, Simple.const (Reg_width_const.tagged_immediate c), name
  | Symbol s ->
    let acc, simple = use_of_symbol_as_simple acc s in
    acc, simple, name
  | Dynamically_computed _ ->
    Misc.fatal_errorf "Declaring a computed constant %s" name

let close_const acc const =
  let acc, simple, name = close_const0 acc const in
  let named = Named.create_simple simple in
  acc, named, name

let find_simple_from_id env id =
  match Env.find_var_exn env id with
  | exception Not_found ->
    Misc.fatal_errorf
      "find_simple_from_id: Cannot find [Ident] %a in environment"
      Ident.print id
  | var ->
    match Env.find_simple_to_substitute_exn env id with
    | exception Not_found -> Simple.var var
    | simple -> simple

(* CR mshinwell: Avoid the double lookup *)
let find_simple acc env (simple : Ilambda.simple) =
  match simple with
  | Const const ->
    let acc, simple, _ = close_const0 acc const in
    acc, simple
  | Var id -> acc, find_simple_from_id env id

let find_simples acc env ids =
  List.fold_left_map
    (fun acc id -> find_simple acc env id)
    acc
    ids

let close_c_call acc ~let_bound_var (prim : Primitive.description)
      ~(args : Simple.t list) exn_continuation dbg
      (k : Acc.t -> Named.t option -> Acc.t * Expr_with_acc.t)
  : Acc.t * Expr_with_acc.t =
  (* We always replace the original Ilambda [Let] with an Flambda
     expression, so we call [k] with [None], to get just the closure-converted
     body of that [Let]. *)
  let cost_metrics_of_body, acc, body =
    Acc.measure_cost_metrics acc ~f:(fun acc -> k acc None)
  in
  let return_continuation, needs_wrapper =
    match Expr.descr body with
    | Apply_cont apply_cont
      when
        Simple.List.equal (Apply_cont_expr.args apply_cont)
          [Simple.var let_bound_var]
        && Option.is_none (Apply_cont_expr.trap_action apply_cont) ->
      Apply_cont_expr.continuation apply_cont, false
    | _ -> Continuation.create (), true
  in
  let param_arity =
    List.map LC.kind_of_primitive_native_repr prim.prim_native_repr_args
  in
  let return_kind =
    LC.kind_of_primitive_native_repr prim.prim_native_repr_res
  in
  let return_arity = [return_kind] in
  let call_kind =
    Call_kind.c_call ~alloc:prim.prim_alloc ~param_arity ~return_arity
  in
  let call_symbol =
    let prim_name =
      if String.equal prim.prim_native_name "" then prim.prim_name
      else prim.prim_native_name
    in
    (* CR mshinwell: fix "extern" mess (see Un_cps) *)
    Symbol.create (Compilation_unit.external_symbols ())
      (Linkage_name.create prim_name)
  in
  let call args =
    (* Some C primitives have implementations within Flambda itself. *)
    match prim.prim_native_name with
    | "caml_int64_float_of_bits_unboxed"
      (* There is only one case where this operation is not the identity:
         on 32-bit pre-EABI ARM platforms.  It is very unlikely anyone would
         still be using one of those, but just in case, we only optimise this
         primitive on 64-bit systems.  (There is no easy way here of
         detecting just the specific ARM case in question.) *)
      when
        begin match Targetint.num_bits with
        | Thirty_two -> false
        | Sixty_four -> true
        end
      ->
      if prim.prim_arity <> 1 then
        Misc.fatal_errorf "Expected arity one for %s" prim.prim_native_name
      else
        begin match prim.prim_native_repr_args, prim.prim_native_repr_res with
        | [Unboxed_integer Pint64], Unboxed_float ->
          begin match args with
          | [arg] ->
            let result = Variable.create "reinterpreted_int64" in
            let result' = Var_in_binding_pos.create result Name_mode.normal in
            let bindable = Bindable_let_bound.singleton result' in
            let prim = P.Unary (Reinterpret_int64_as_float, arg) in
            let return_result =
              Apply_cont.create return_continuation
                ~args:[Simple.var result]
                ~dbg
            in
            let acc, return_result_expr =
              Expr_with_acc.create_apply_cont acc return_result
            in
            Let_with_acc.create acc bindable (Named.create_prim prim dbg)
              ~body:return_result_expr
              ~free_names_of_body:(Known (Apply_cont.free_names return_result))
            |> Expr_with_acc.create_let
          | [] | _::_ ->
            Misc.fatal_errorf "Expected one arg for %s" prim.prim_native_name
          end
        | _, _ ->
          Misc.fatal_errorf "Wrong argument and/or result kind(s) for %s"
            prim.prim_native_name
        end
    | _ ->
      let acc, callee = use_of_symbol_as_simple acc call_symbol in
      let apply =
        Apply.create ~callee
          ~continuation:(Return return_continuation)
          exn_continuation
          ~args
          ~call_kind
          dbg
          ~inline:Default_inline
          ~inlining_state:(Inlining_state.default)
      in
      Expr_with_acc.create_apply acc apply
  in
  let (acc, call) : Acc.t * Expr_with_acc.t =
    List.fold_left2
      (fun (call : Simple.t list -> Acc.t * Expr_with_acc.t)
        arg (arg_repr : Primitive.native_repr) ->
        let unbox_arg : P.unary_primitive option =
          match arg_repr with
          | Same_as_ocaml_repr -> None
          | Unboxed_float -> Some (P.Unbox_number Naked_float)
          | Unboxed_integer Pnativeint -> Some (P.Unbox_number Naked_nativeint)
          | Unboxed_integer Pint32 -> Some (P.Unbox_number Naked_int32)
          | Unboxed_integer Pint64 -> Some (P.Unbox_number Naked_int64)
          | Untagged_int -> Some (P.Unbox_number Untagged_immediate)
        in
        match unbox_arg with
        | None -> (fun args -> call (arg :: args))
        | Some named ->
          (fun args ->
             let unboxed_arg = Variable.create "unboxed" in
             let unboxed_arg' =
               VB.create unboxed_arg Name_mode.normal
             in
             let acc, body = call ((Simple.var unboxed_arg) :: args) in
             let named =
               Named.create_prim (Unary (named, arg)) dbg
             in
             Let_with_acc.create acc
               (Bindable_let_bound.singleton unboxed_arg')
               named
               ~body
               ~free_names_of_body:Unknown
             |> Expr_with_acc.create_let))
      call
      args
      prim.prim_native_repr_args
      []
  in
  let box_unboxed_returns acc ~let_bound_var ~box_return_value body =
    let let_bound_var' = VB.create let_bound_var Name_mode.normal in
    let handler_param = Variable.rename let_bound_var in
    let named =
      Named.create_prim
        (Unary (box_return_value, Simple.var handler_param))
        dbg
    in
    let acc, expr =
      Let_with_acc.create acc (Bindable_let_bound.singleton let_bound_var')
        named
        ~body
        ~free_names_of_body:Unknown
      |> Expr_with_acc.create_let
    in
    acc, expr, handler_param
  in
  let wrap_c_call acc ~handler_param ~code_after_call c_call =
    let cost_metrics_of_handler, acc, after_call =
      Acc.measure_cost_metrics acc ~f:(fun acc ->
        let return_kind =
          Flambda_kind.With_subkind.create return_kind Anything
        in
        let params = [Kinded_parameter.create handler_param return_kind] in
        Continuation_handler_with_acc.create acc params
          ~handler:code_after_call
          (* Here and elsewhere in this pass, we specify [Unknown] for
             free name sets like this, since the information isn't needed
             until the translation to Cmm -- by which point Simplify will
             have rebuilt the term and provided the free names. *)
          ~free_names_of_handler:Unknown
          ~is_exn_handler:false
      )
    in
    Let_cont_with_acc.create_non_recursive acc return_continuation after_call
      ~body:c_call
      ~free_names_of_body:Unknown
      ~cost_metrics_of_handler
  in
  let keep_body ~cost_metrics_of_body acc =
    Acc.with_cost_metrics
      (Cost_metrics.(+) (Acc.cost_metrics acc) cost_metrics_of_body)
      acc
  in
  let box_return_value =
    match prim.prim_native_repr_res with
    | Same_as_ocaml_repr -> None
    | Unboxed_float -> Some (P.Box_number Naked_float)
    | Unboxed_integer Pnativeint -> Some (P.Box_number Naked_nativeint)
    | Unboxed_integer Pint32 -> Some (P.Box_number Naked_int32)
    | Unboxed_integer Pint64 -> Some (P.Box_number Naked_int64)
    | Untagged_int -> Some (P.Box_number Untagged_immediate)
  in
  match box_return_value with
  | None ->
    if needs_wrapper then
      let acc = keep_body ~cost_metrics_of_body acc in
      wrap_c_call acc ~handler_param:let_bound_var ~code_after_call:body call
    else
      (* Here the body is discarded. It might be useful to explicitly remove
         anything that has been added to the acc while converting the body.
         However, as we are hitting this code only when body is a goto
         continuation where the only parameter is [let_bound_var] this
         operation would be a noop and we can skip it.
      *)
      acc, call
  | Some box_return_value ->
    let acc = keep_body ~cost_metrics_of_body acc in
    let acc, code_after_call, handler_param =
      box_unboxed_returns acc ~let_bound_var ~box_return_value body
    in
    wrap_c_call acc ~handler_param ~code_after_call call

let close_exn_continuation acc env
      (exn_continuation : Ilambda.exn_continuation) =
  let acc, extra_args =
    List.fold_left_map (fun acc (simple, kind) ->
        let acc, simple = find_simple acc env simple in
        acc, (simple, LC.value_kind kind))
      acc
      exn_continuation.extra_args
  in
  acc,
  Exn_continuation.create ~exn_handler:exn_continuation.exn_handler ~extra_args

let close_primitive acc env ~let_bound_var named (prim : Lambda.primitive) ~args
      loc (exn_continuation : Ilambda.exn_continuation option)
      (k : Acc.t -> Named.t option -> Acc.t * Expr_with_acc.t)
  : Acc.t * Expr_with_acc.t =
  let acc, exn_continuation =
    match exn_continuation with
    | None -> acc, None
    | Some exn_continuation ->
      let acc, cont = close_exn_continuation acc env exn_continuation in
      acc, Some (cont)
  in
  let acc, args = find_simples acc env args in
  let dbg = Debuginfo.from_location loc in
  match prim, args with
  | Pccall prim, args ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf "Pccall is missing exception continuation: %a"
          Ilambda.print_named named
      | Some exn_continuation -> exn_continuation
    in
    close_c_call acc ~let_bound_var prim ~args exn_continuation dbg k
  | Pgetglobal id, [] ->
    let is_predef_exn = Ident.is_predef id in
    if not (is_predef_exn || not (Ident.same id (Env.current_unit_id env)))
    then begin
      Misc.fatal_errorf "Non-predef Pgetglobal %a in the same unit"
        Ident.print id
    end;
    let acc, simple = symbol_for_ident acc env id in
    let named = Named.create_simple simple in
    k acc (Some named)
  | Praise raise_kind, [_] ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf "Praise is missing exception continuation: %a"
          Ilambda.print_named named
      | Some exn_continuation -> exn_continuation
    in
    let exn_handler = Exn_continuation.exn_handler exn_continuation in
    let args =
      (* CR mshinwell: Share with [Lambda_to_flambda_primitives_helpers] *)
      let extra_args =
        List.map (fun (simple, _kind) -> simple)
          (Exn_continuation.extra_args exn_continuation)
      in
      args @ extra_args
    in
    let raise_kind = Some (LC.raise_kind raise_kind) in
    let trap_action = Trap_action.Pop { exn_handler; raise_kind; } in
    let apply_cont =
      Apply_cont.create ~trap_action exn_handler ~args ~dbg
    in
    (* Since raising of an exception doesn't terminate, we don't call [k]. *)
    Expr_with_acc.create_apply_cont acc apply_cont
  | prim, args ->
    Lambda_to_flambda_primitives.convert_and_bind acc exn_continuation
      ~backend:(Env.backend env)
      ~register_const_string:(fun acc -> register_const_string acc)
      prim ~args dbg k

let close_trap_action_opt trap_action =
  Option.map (fun (trap_action : Ilambda.trap_action) : Trap_action.t ->
      match trap_action with
      | Push { exn_handler; } -> Push { exn_handler; }
      | Pop { exn_handler; } ->
        Pop { exn_handler; raise_kind = None; })
    trap_action

let rec close acc env (ilam : Ilambda.t) : Acc.t * Expr_with_acc.t =
  match ilam with
  | Let (id, user_visible, _kind, defining_expr, body) ->
    (* CR mshinwell: Remove [kind] on the Ilambda terms? *)
    let body_env, var = Env.add_var_like env id user_visible in
    let cont acc (defining_expr : Named.t option) =
      let body_env =
        match defining_expr with
        | Some (Simple simple) ->
          Env.add_simple_to_substitute body_env id simple
        | Some _ | None -> body_env
      in
      (* CR pchambart: Not tail ! *)
      let acc, body = close acc body_env body in
      match defining_expr with
      | None -> acc, body
      | Some defining_expr ->
        let var = VB.create var Name_mode.normal in
        Let_with_acc.create acc (Bindable_let_bound.singleton var) defining_expr
          ~body ~free_names_of_body:Unknown
        |> Expr_with_acc.create_let
    in
    close_named acc env ~let_bound_var:var defining_expr cont
  | Let_mutable _ ->
    Misc.fatal_error "[Let_mutable] should have been removed by \
      [Eliminate_mutable_vars]"
  | Let_rec (defs, body) -> close_let_rec acc env ~defs ~body
  | Let_cont { name; is_exn_handler; params; recursive; body;
      handler; } ->
    if is_exn_handler then begin
      match recursive with
      | Nonrecursive -> ()
      | Recursive ->
        Misc.fatal_errorf "[Let_cont]s marked as exception handlers must \
            be [Nonrecursive]: %a"
          Ilambda.print ilam
    end;
    let params_with_kinds = params in
    let handler_env, params =
      Env.add_vars_like env
        (List.map (fun (param, user_visible, _kind) -> param, user_visible)
          params)
    in
    let params =
      List.map2 (fun param (_, _, kind) ->
          Kinded_parameter.create param (LC.value_kind kind))
        params
        params_with_kinds
    in
    let cost_metrics_of_handler, acc, handler =
      Acc.measure_cost_metrics acc ~f:(fun acc ->
        close acc handler_env handler)
    in
    let acc, handler =
      Continuation_handler_with_acc.create acc params ~handler
        ~free_names_of_handler:Unknown
        ~is_exn_handler
    in
    let acc, body = close acc env body in
    begin match recursive with
    | Nonrecursive ->
      Let_cont_with_acc.create_non_recursive acc name handler ~body
        ~free_names_of_body:Unknown ~cost_metrics_of_handler
    | Recursive ->
      let handlers = Continuation.Map.singleton name handler in
      Let_cont_with_acc.create_recursive acc handlers ~body
        ~cost_metrics_of_handlers:cost_metrics_of_handler
    end
  | Apply { kind; func; args; continuation; exn_continuation;
      loc; should_be_tailcall = _; inlined; specialised = _; } ->
    let acc, call_kind =
      match kind with
      | Function -> acc, Call_kind.indirect_function_call_unknown_arity ()
      | Method { kind; obj; } ->
        let acc, obj = find_simple acc env obj in
        acc,
        Call_kind.method_call (LC.method_kind kind) ~obj
    in
    let acc, exn_continuation =
      close_exn_continuation acc env exn_continuation
    in
    let callee = find_simple_from_id env func in
    let acc, args = find_simples acc env args in
    let apply =
      Apply.create ~callee
        ~continuation:(Return continuation)
        exn_continuation
        ~args
        ~call_kind
        (Debuginfo.from_location loc)
        ~inline:(LC.inline_attribute inlined)
        ~inlining_state:(Inlining_state.default)
    in
    Expr_with_acc.create_apply acc apply
  | Apply_cont (cont, trap_action, args) ->
    let acc, args = find_simples acc env args in
    let trap_action = close_trap_action_opt trap_action in
    let apply_cont =
      Apply_cont.create ?trap_action cont ~args ~dbg:Debuginfo.none
    in
    Expr_with_acc.create_apply_cont acc apply_cont
  | Switch (scrutinee, sw) ->
    let scrutinee = Simple.name (Env.find_name env scrutinee) in
    let untagged_scrutinee = Variable.create "untagged" in
    let untagged_scrutinee' =
      VB.create untagged_scrutinee Name_mode.normal
    in
    let untag =
      Named.create_prim
        (Unary (Unbox_number Untagged_immediate, scrutinee))
        Debuginfo.none
    in
    let acc, arms =
      List.fold_left_map (fun acc (case, cont, trap_action, args) ->
          let trap_action = close_trap_action_opt trap_action in
          let acc, args = find_simples acc env args in
          acc,
          (Target_imm.int (Targetint.OCaml.of_int case),
            Apply_cont.create ?trap_action cont ~args
              ~dbg:Debuginfo.none))
        acc
        sw.consts
    in
    match arms, sw.failaction with
    | [case, action], Some (default_action, default_trap_action, default_args)
        when sw.numconsts >= 3 ->
      (* Avoid enormous switches, where every arm goes to the same place
         except one, that arise from single-arm [Lambda] switches with a
         default case.  (Seen in code generated by ppx_compare for variants,
         which exhibited quadratic size blowup.) *)
      let compare =
        Named.create_prim
          (Binary (Phys_equal (Flambda_kind.naked_immediate, Eq),
            Simple.var untagged_scrutinee,
            Simple.const (Reg_width_const.naked_immediate case)))
          Debuginfo.none
      in
      let comparison_result = Variable.create "eq" in
      let comparison_result' = VB.create comparison_result Name_mode.normal in
      let acc, default_action =
        let acc, args = find_simples acc env default_args in
        let trap_action = close_trap_action_opt default_trap_action in
        acc,
        Apply_cont.create ?trap_action default_action ~args
          ~dbg:Debuginfo.none
      in
      let acc, switch =
        let scrutinee = Simple.var comparison_result in
        Expr_with_acc.create_switch acc (
          Switch.if_then_else ~scrutinee
            ~if_true:action
            ~if_false:default_action)
      in
      let acc, body =
        Let_with_acc.create acc (Bindable_let_bound.singleton comparison_result')
          compare ~body:switch ~free_names_of_body:Unknown
        |> Expr_with_acc.create_let
      in
      Let_with_acc.create acc (Bindable_let_bound.singleton untagged_scrutinee')
        untag ~body ~free_names_of_body:Unknown
      |> Expr_with_acc.create_let
    | _, _ ->
      let acc, arms =
        match sw.failaction with
        | None -> acc, Target_imm.Map.of_list arms
        | Some (default, trap_action, args) ->
          Numbers.Int.Set.fold (fun case (acc, cases) ->
              let case = Target_imm.int (Targetint.OCaml.of_int case) in
              if Target_imm.Map.mem case cases then acc, cases
              else
                let acc, args = find_simples acc env args in
                let trap_action = close_trap_action_opt trap_action in
                let default =
                  Apply_cont.create ?trap_action default ~args
                    ~dbg:Debuginfo.none
                in
                acc,
                Target_imm.Map.add case default cases)
            (Numbers.Int.zero_to_n (sw.numconsts - 1))
            (acc, Target_imm.Map.of_list arms)
      in
      if Target_imm.Map.is_empty arms then
        Expr_with_acc.create_invalid acc ()
      else
        let scrutinee = Simple.var untagged_scrutinee in
        let acc, body =
          match Target_imm.Map.get_singleton arms with
          | Some (_discriminant, action) ->
            Expr_with_acc.create_apply_cont acc action
          | None ->
            Expr_with_acc.create_switch acc (Switch.create ~scrutinee ~arms)
        in
        Let_with_acc.create acc
          (Bindable_let_bound.singleton untagged_scrutinee')
          untag ~body ~free_names_of_body:Unknown
        |> Expr_with_acc.create_let

and close_named acc env ~let_bound_var (named : Ilambda.named)
      (k : Acc.t -> Named.t option -> Acc.t * Expr_with_acc.t)
  : Acc.t * Expr_with_acc.t =
  match named with
  | Simple (Var id) ->
    let acc, simple =
      if not (Ident.is_predef id) then acc, Simple.var (Env.find_var env id)
      else symbol_for_ident acc env id
    in
    let named = Named.create_simple simple in
    k acc (Some named)
  | Simple (Const cst) ->
    let acc, named, _name = close_const acc cst in
    k acc (Some named)
  | Prim { prim; args; loc; exn_continuation; } ->
    close_primitive acc env ~let_bound_var named prim ~args loc
      exn_continuation k
  | Assign _ | Mutable_read _ ->
    Misc.fatal_error "[Assign] and [Mutable_read] should have been removed \
      by [Eliminate_mutable_vars]"

and close_let_rec acc env ~defs ~body =
  let env =
    List.fold_right (fun (id, _) env ->
        let env, _var = Env.add_var_like env id User_visible in
        env)
      defs env
  in
  let recursive_functions = Ilambda.recursive_functions defs in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let function_declarations =
    List.map (function (let_rec_ident,
            ({ kind; return_continuation; exn_continuation;
               params; return; body; free_idents_of_body;
               attr; loc; stub;
             } : Ilambda.function_declaration)) ->
        let closure_id =
          Closure_id.wrap compilation_unit
            (Variable.create_with_same_name_as_ident let_rec_ident)
        in
        let recursive : Recursive.t =
          if Ident.Set.mem let_rec_ident recursive_functions then
            Recursive
          else
            Non_recursive
        in
        let function_declaration =
          Function_decl.create ~let_rec_ident:(Some let_rec_ident)
            ~closure_id ~kind ~params ~return ~return_continuation
            ~exn_continuation ~body ~attr ~loc ~free_idents_of_body ~stub
            recursive
        in
        function_declaration)
      defs
  in
  let closure_vars =
    List.fold_left (fun closure_vars decl ->
        let closure_var =
          VB.create (Env.find_var env (Function_decl.let_rec_ident decl))
            Name_mode.normal
        in
        let closure_id = Function_decl.closure_id decl in
        Closure_id.Map.add closure_id closure_var closure_vars)
      Closure_id.Map.empty
      function_declarations
  in
  let acc, set_of_closures =
    close_functions acc env (Function_decls.create function_declarations)
  in
  (* CR mshinwell: We should maybe have something more elegant here *)
  let generated_closures =
    Closure_id.Set.diff
      (Closure_id.Map.keys (Function_declarations.funs (
        Set_of_closures.function_decls set_of_closures)))
      (Closure_id.Map.keys closure_vars)
  in
  let closure_vars =
    Closure_id.Set.fold (fun closure_id closure_vars ->
        let closure_var =
          VB.create (Variable.create "generated") Name_mode.normal
        in
        Closure_id.Map.add closure_id closure_var closure_vars)
      generated_closures
      closure_vars
  in
  let closure_vars =
    List.map (fun (closure_id, _) ->
        Closure_id.Map.find closure_id closure_vars)
      (Function_declarations.funs_in_order (
          Set_of_closures.function_decls set_of_closures)
        |> Closure_id.Lmap.bindings)
  in
  let acc, body = close acc env body in
  let named = Named.create_set_of_closures set_of_closures in
  Let_with_acc.create acc
    (Bindable_let_bound.set_of_closures ~closure_vars)
    named
    ~body ~free_names_of_body:Unknown
  |> Expr_with_acc.create_let

and close_functions acc external_env function_declarations =
  let compilation_unit = Compilation_unit.get_current_exn () in
  let var_within_closures_from_idents =
    Ident.Set.fold (fun id map ->
        (* Filter out predefined exception identifiers, since they will be
           turned into symbols when we closure-convert the body. *)
        if Ident.is_predef id then map
        else
          let var = Variable.create_with_same_name_as_ident id in
          Ident.Map.add id (Var_within_closure.wrap compilation_unit var) map)
      (Function_decls.all_free_idents function_declarations)
      Ident.Map.empty
  in
  let func_decl_list = Function_decls.to_list function_declarations in
  let closure_ids_from_idents =
    List.fold_left (fun map decl ->
        let id = Function_decl.let_rec_ident decl in
        let closure_id = Function_decl.closure_id decl in
        Ident.Map.add id closure_id map)
      Ident.Map.empty
      func_decl_list
  in
  let acc, funs =
    List.fold_left (fun (acc, by_closure_id) function_decl ->
        let _, acc, expr =
          Acc.measure_cost_metrics acc ~f:(fun acc ->
            close_one_function acc ~external_env ~by_closure_id function_decl
              ~var_within_closures_from_idents ~closure_ids_from_idents
              function_declarations)
        in
        acc, expr)
      (acc, Closure_id.Map.empty)
      func_decl_list
  in
  (* CR lmaurer: funs has arbitrary order (ultimately coming from
     function_declarations) *)
  let funs =
    Closure_id.Lmap.of_list (Closure_id.Map.bindings funs)
  in
  let function_decls = Function_declarations.create funs in
  let closure_elements =
    Ident.Map.fold (fun id var_within_closure map ->
        let external_var = Simple.var (Env.find_var external_env id) in
        Var_within_closure.Map.add var_within_closure external_var map)
      var_within_closures_from_idents
      Var_within_closure.Map.empty
  in
  acc,
  Set_of_closures.create function_decls ~closure_elements

and close_one_function acc ~external_env ~by_closure_id decl
      ~var_within_closures_from_idents ~closure_ids_from_idents
      function_declarations =
  let acc = Acc.with_free_names Name_occurrences.empty acc in
  let body = Function_decl.body decl in
  let loc = Function_decl.loc decl in
  let dbg = Debuginfo.from_location loc in
  let params = Function_decl.params decl in
  let return = Function_decl.return decl in
  let recursive = Function_decl.recursive decl in
  let my_closure = Variable.create "my_closure" in
  let closure_id = Function_decl.closure_id decl in
  let my_closure_id = closure_id in
  let our_let_rec_ident = Function_decl.let_rec_ident decl in
  let contains_closures = Function_decl.contains_closures decl in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let code_id =
    Code_id.create ~name:(Closure_id.to_string closure_id) compilation_unit
  in
  let is_curried =
    match Function_decl.kind decl with
    | Curried -> true
    | Tupled -> false
  in
  (* The free variables are:
     - The parameters: direct substitution by [Variable]s
     - The function being defined: accessible through [my_closure]
     - Other functions in the set being defined: accessible from [my_closure]
       then a [Select_closure]
     - Other free variables: accessible using [Project_var] from
       [my_closure].
     Note that free variables corresponding to predefined exception
     identifiers have been filtered out by [close_functions], above.
  *)
  let var_within_closures_to_bind, var_within_closures_for_idents =
    Ident.Map.fold
      (fun id var_within_closures_for_idents (to_bind, var_for_ident) ->
        let var = Variable.create_with_same_name_as_ident id in
        Variable.Map.add var var_within_closures_for_idents to_bind,
          Ident.Map.add id var var_for_ident)
      var_within_closures_from_idents
      (Variable.Map.empty, Ident.Map.empty)
  in
  (* CR mshinwell: Remove "project_closure" names *)
  let project_closure_to_bind, vars_for_project_closure =
    List.fold_left (fun (to_bind, vars_for_idents) function_decl ->
        let let_rec_ident = Function_decl.let_rec_ident function_decl in
        let to_bind, var =
          if Ident.same our_let_rec_ident let_rec_ident && is_curried then
            (* When the function being compiled is tupled, my_closure
               points to the curried version but let_rec_ident is called
               with tuple arguments, so the correct closure to bind
               is the one in the closure_ids_from_idents map.
            *)
            to_bind, my_closure  (* my_closure is already bound *)
          else
            let variable =
              Variable.create_with_same_name_as_ident let_rec_ident
            in
            let closure_id =
              Ident.Map.find let_rec_ident closure_ids_from_idents
            in
            Variable.Map.add variable closure_id to_bind, variable
        in
        to_bind,
        Ident.Map.add let_rec_ident var vars_for_idents)
      (Variable.Map.empty, Ident.Map.empty)
      (Function_decls.to_list function_declarations)
  in
  let closure_env_without_parameters =
    let empty_env = Env.clear_local_bindings external_env in
    Env.add_var_map (Env.add_var_map empty_env var_within_closures_for_idents)
      vars_for_project_closure
  in
  let closure_env =
    List.fold_right (fun (id, _) env ->
        let env, _var = Env.add_var_like env id User_visible in
        env)
      params
      closure_env_without_parameters
  in
  (* CR-someday pchambart: eta-expansion wrappers for primitives are
     not marked as stubs but certainly should be. *)
  let stub = Function_decl.stub decl in
  let param_vars =
    List.map (fun (id, kind) -> Env.find_var closure_env id, kind) params
  in
  let params =
    List.map (fun (var, kind) ->
        Kinded_parameter.create var (LC.value_kind kind))
      param_vars
  in
  let acc, body =
    try close acc closure_env body
    with Misc.Fatal_error -> begin
      if !Clflags.flambda_context_on_error then begin
        Format.eprintf "\n%sContext is:%s closure converting \
          function@ with [our_let_rec_ident] %a (closure ID %a)@ \
          and body:@ %a"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Ident.print our_let_rec_ident
          Closure_id.print closure_id
          Ilambda.print body
      end;
      raise Misc.Fatal_error
    end
  in
  let my_closure' = Simple.var my_closure in
  let acc, body =
    (* CR mshinwell: These Select_closure operations should maybe be inserted
       at the point of use rather than at the top of the function.  We should
       also check the behaviour of the backend w.r.t. CSE of projections from
       closures. *)
    Variable.Map.fold (fun var closure_id (acc, body) ->
        let move : Flambda_primitive.unary_primitive =
          Select_closure {
            move_from = my_closure_id;
            move_to = closure_id;
          }
        in
        let var = VB.create var Name_mode.normal in
        let named =
          Named.create_prim
            (Unary (move, my_closure'))
            Debuginfo.none
        in
        Let_with_acc.create acc (Bindable_let_bound.singleton var)
          named
          ~body ~free_names_of_body:Unknown
        |> Expr_with_acc.create_let)
      project_closure_to_bind
      (acc, body)
  in
  let acc =
    Variable.Map.fold
      (fun _var closure_var acc ->
         Acc.add_closure_var_to_free_names ~closure_var acc)
      var_within_closures_to_bind
      acc
  in
  let acc, body =
    Variable.Map.fold (fun var var_within_closure (acc, body) ->
        let var = VB.create var Name_mode.normal in
        let named =
          Named.create_prim
             (Unary (Project_var {
                project_from = my_closure_id;
                var = var_within_closure;
              }, my_closure'))
             Debuginfo.none
        in
        Let_with_acc.create acc (Bindable_let_bound.singleton var)
          named
          ~body
          ~free_names_of_body:Unknown
        |> Expr_with_acc.create_let)
      var_within_closures_to_bind
      (acc, body)
  in
  let cost_metrics = Acc.cost_metrics acc in
  let acc, exn_continuation =
    close_exn_continuation acc external_env
      (Function_decl.exn_continuation decl)
  in
  let inline : Inline_attribute.t =
    (* We make a decision based on [fallback_inlining_heuristic] here to try
       to mimic Closure's behaviour as closely as possible, particularly
       when there are functions involving constant closures, which are not
       lifted during Closure (but will prevent inlining) but will likely have
       been lifted by our other check in [Inlining_cost] (thus preventing us
       seeing they were originally there). *)
    if contains_closures
      && !Clflags.Flambda.Expert.fallback_inlining_heuristic
    then Never_inline
    else LC.inline_attribute (Function_decl.inline decl)
  in
  let params_and_body =
    Function_params_and_body.create
      ~return_continuation:(Function_decl.return_continuation decl)
      exn_continuation params ~dbg ~body ~my_closure ~free_names_of_body:Unknown
  in
  let params_arity = Kinded_parameter.List.arity_with_subkinds params in
  let is_tupled =
    match Function_decl.kind decl with
    | Curried -> false
    | Tupled -> true
  in
  let fun_decl =
    Function_declaration.create ~code_id
      ~dbg
      ~is_tupled
  in
  let code =
    Code.create
      code_id
      ~params_and_body:
        (Present (params_and_body, Acc.free_names_of_current_function acc))
      ~params_arity
      ~result_arity:[LC.value_kind return]
      ~stub
      ~inline
      ~is_a_functor:(Function_decl.is_a_functor decl)
      ~recursive
      ~newer_version_of:None
      ~cost_metrics
      ~inlining_arguments:Inlining_arguments.unknown
  in
  Acc.add_code ~code_id ~code acc,
  Closure_id.Map.add my_closure_id fun_decl by_closure_id

let ilambda_to_flambda ~backend ~module_ident ~module_block_size_in_words
      (ilam : Ilambda.program) =
  let module Backend = (val backend : Flambda_backend_intf.S) in
  let env = Env.empty ~backend in
  let module_symbol =
    Backend.symbol_for_global' (
      Ident.create_persistent (Ident.name module_ident))
  in
  let module_block_tag = Tag.Scannable.zero in
  let module_block_var = Variable.create "module_block" in
  let return_cont = Continuation.create ~sort:Toplevel_return () in
  let acc = Acc.empty in
  let acc, load_fields_body =
    let field_vars =
      List.init module_block_size_in_words (fun pos ->
        let pos_str = string_of_int pos in
        pos, Variable.create ("field_" ^ pos_str))
    in
    let acc, body =
      let static_const : Static_const.t =
        let field_vars =
          List.map (fun (_, var) : Static_const.Field_of_block.t ->
              Dynamically_computed var)
            field_vars
        in
        Block (module_block_tag, Immutable, field_vars)
      in
      let acc, arg = use_of_symbol_as_simple acc module_symbol in
      let acc, return =
        (* Module initialisers return unit, but since that is taken care of
           during Cmm generation, we can instead "return" [module_symbol]
           here to ensure that its associated "let symbol" doesn't get
           deleted. *)
        Apply_cont.create return_cont
          ~args:[arg]
          ~dbg:Debuginfo.none
        |> Expr_with_acc.create_apply_cont acc
      in
      let bound_symbols =
        Bound_symbols.singleton
          (Bound_symbols.Pattern.block_like module_symbol)
      in
      let named =
        Named.create_static_consts
          (Static_const.Group.create [static_const])
      in
      Let_with_acc.create acc
        (Bindable_let_bound.symbols bound_symbols Syntactic)
        named
        ~body:return
        ~free_names_of_body:Unknown
      |> Expr_with_acc.create_let
    in
    let block_access : P.Block_access_kind.t =
      Values {
        tag = Tag.Scannable.zero;
        size = Known (Targetint.OCaml.of_int module_block_size_in_words);
        field_kind = Any_value;
      }
    in
    List.fold_left (fun (acc, body) (pos, var) ->
        let var = VB.create var Name_mode.normal in
        let pos = Target_imm.int (Targetint.OCaml.of_int pos) in
        let named =
          Named.create_prim
             (Binary (
                Block_load (block_access, Immutable),
                Simple.var module_block_var,
                Simple.const (Reg_width_const.tagged_immediate pos)))
             Debuginfo.none
        in
        Let_with_acc.create acc
          (Bindable_let_bound.singleton var)
          named
          ~body
          ~free_names_of_body:Unknown
        |> Expr_with_acc.create_let)
      (acc, body) (List.rev field_vars)
  in
  let cost_metrics_of_handler, acc, load_fields_cont_handler =
    Acc.measure_cost_metrics acc ~f:(fun acc ->
      let param =
        Kinded_parameter.create module_block_var K.With_subkind.any_value
      in
      Continuation_handler_with_acc.create acc [param] ~handler:load_fields_body
        ~free_names_of_handler:Unknown
        ~is_exn_handler:false
    )
  in
  let acc, body =
    (* This binds the return continuation that is free (or, at least, not bound)
       in the incoming Ilambda code. The handler for the continuation receives a
       tuple with fields indexed from zero to [module_block_size_in_words]. The
       handler extracts the fields; the variables bound to such fields are then
       used to define the module block symbol. *)
    let acc, body = close acc env ilam.expr in
    Let_cont_with_acc.create_non_recursive acc ilam.return_continuation
      load_fields_cont_handler
      ~body
      ~free_names_of_body:Unknown
      ~cost_metrics_of_handler
  in
  begin match ilam.exn_continuation.extra_args with
  | [] -> ()
  | _::_ ->
    Misc.fatal_error "Ilambda toplevel exception continuation cannot have \
      extra arguments"
  end;
  let exn_continuation = ilam.exn_continuation.exn_handler in
  let acc, body =
    Code_id.Map.fold (fun code_id code (acc, body) ->
      let bound_symbols =
          Bound_symbols.singleton (Bound_symbols.Pattern.code code_id)
        in
        let static_const : Static_const.t =
          Code code
        in
        let defining_expr =
          Static_const.Group.create [static_const]
          |> Named.create_static_consts
        in
        Let_with_acc.create acc
          (Bindable_let_bound.symbols bound_symbols Syntactic)
          defining_expr ~body ~free_names_of_body:Unknown
        |> Expr_with_acc.create_let)
      (Acc.code acc)
      (acc, body)
  in
  (* We must make sure there is always an outer [Let_symbol] binding so that
     lifted constants not in the scope of any other [Let_symbol] binding get
     put into the term and not dropped.  Adding this extra binding, which
     will actually be removed by the simplifier, avoids a special case. *)
  let acc =
    match Acc.declared_symbols acc with
    | _::_ -> acc
    | [] ->
      let acc, (_sym : Symbol.t) =
        register_const0 acc
          (Static_const.Block (Tag.Scannable.zero, Immutable, []))
          "first_const"
      in
      acc
  in
  let acc, body =
    List.fold_left (fun (acc, body) (symbol, static_const) ->
      let bound_symbols =
        Bound_symbols.singleton (Bound_symbols.Pattern.block_like symbol)
      in
      let defining_expr =
        Static_const.Group.create [static_const]
        |> Named.create_static_consts
      in
      Let_with_acc.create acc
        (Bindable_let_bound.symbols bound_symbols Syntactic)
        defining_expr ~body ~free_names_of_body:Unknown
      |> Expr_with_acc.create_let)
      (acc, body)
      (Acc.declared_symbols acc)
  in
  ignore (acc : Acc.t);
  Flambda_unit.create ~return_continuation:return_cont ~exn_continuation
    ~body ~module_symbol ~used_closure_vars:Unknown

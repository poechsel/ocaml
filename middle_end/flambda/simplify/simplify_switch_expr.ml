(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

let rebuild_switch ~simplify_let dacc ~arms ~scrutinee ~scrutinee_ty uacc
      ~after_rebuild =
  let new_let_conts, arms, identity_arms, not_arms =
    Target_imm.Map.fold
      (fun arm (action, use_id, arity)
           (new_let_conts, arms, identity_arms, not_arms) ->
        match
          EB.add_wrapper_for_switch_arm uacc action
            ~use_id (Flambda_arity.With_subkinds.of_arity arity)
        with
        | Apply_cont action ->
          let action =
            (* First try to absorb any [Apply_cont] expression that forms the
               entirety of the arm's action (via an intermediate zero-arity
               continuation without trap action) into the [Switch] expression
               itself. *)
            if not (Apply_cont.is_goto action) then Some action
            else
              let cont = Apply_cont.continuation action in
              let check_handler ~handler ~action =
                match RE.to_apply_cont handler with
                | Some action -> Some action
                | None -> Some action
              in
              match UE.find_continuation (UA.uenv uacc) cont with
              | Linearly_used_and_inlinable { handler;
                  free_names_of_handler = _; params;
                  cost_metrics_of_handler = _ } ->
                assert (List.length params = 0);
                check_handler ~handler ~action
              | Non_inlinable_zero_arity { handler = Known handler; } ->
                check_handler ~handler ~action
              | Non_inlinable_zero_arity { handler = Unknown; } -> Some action
              | Unreachable _ -> None
              | Non_inlinable_non_zero_arity _
              | Toplevel_or_function_return_or_exn_continuation _ ->
                Misc.fatal_errorf "Inconsistency for %a between \
                    [Apply_cont.is_goto] and continuation environment \
                    in [UA]:@ %a"
                  Continuation.print cont
                  UA.print uacc
          in
          begin match action with
          | None ->
            (* The destination is unreachable; delete the [Switch] arm. *)
            new_let_conts, arms, identity_arms, not_arms
          | Some action ->
            let normal_case ~identity_arms ~not_arms =
              let arms = Target_imm.Map.add arm action arms in
              new_let_conts, arms, identity_arms, not_arms
            in
            (* Now check to see if the arm is of a form that might mean the
               whole [Switch] is either the identity or a boolean NOT. *)
            match Apply_cont.to_one_arg_without_trap_action action with
            | None -> normal_case ~identity_arms ~not_arms
            | Some arg ->
              (* CR-someday mshinwell: Maybe this check should be generalised
                 e.g. to detect
                   | 0 -> apply_cont k x y 1
                   | 1 -> apply_cont k x y 0
              *)
              let [@inline always] const arg =
                match Reg_width_const.descr arg with
                | Tagged_immediate arg ->
                  if Target_imm.equal arm arg then
                    let identity_arms =
                      Target_imm.Map.add arm action identity_arms
                    in
                    normal_case ~identity_arms ~not_arms
                  else if
                    (Target_imm.equal arm Target_imm.bool_true
                      && Target_imm.equal arg Target_imm.bool_false)
                    ||
                      (Target_imm.equal arm Target_imm.bool_false
                        && Target_imm.equal arg Target_imm.bool_true)
                  then
                    let not_arms = Target_imm.Map.add arm action not_arms in
                    normal_case ~identity_arms ~not_arms
                  else
                    normal_case ~identity_arms ~not_arms
                | Naked_immediate _ | Naked_float _ | Naked_int32 _
                | Naked_int64 _ | Naked_nativeint _ ->
                  normal_case ~identity_arms ~not_arms
              in
              Simple.pattern_match arg ~const
                ~name:(fun _ -> normal_case ~identity_arms ~not_arms)
          end
        | New_wrapper (new_cont, new_handler, free_names_of_handler,
            cost_metrics_handler) ->
          let new_let_cont =
            new_cont, new_handler, free_names_of_handler, cost_metrics_handler
          in
          let new_let_conts = new_let_cont :: new_let_conts in
          let action = Apply_cont.goto new_cont in
          let arms = Target_imm.Map.add arm action arms in
          new_let_conts, arms, identity_arms, not_arms)
      arms
      ([], Target_imm.Map.empty, Target_imm.Map.empty, Target_imm.Map.empty)
  in
  let switch_is_identity =
    let arm_discrs = Target_imm.Map.keys arms in
    let identity_arms_discrs = Target_imm.Map.keys identity_arms in
    if not (Target_imm.Set.equal arm_discrs identity_arms_discrs) then
      None
    else
      Target_imm.Map.data identity_arms
      |> List.map Apply_cont.continuation
      |> Continuation.Set.of_list
      |> Continuation.Set.get_singleton
  in
  let switch_is_boolean_not =
    let arm_discrs = Target_imm.Map.keys arms in
    let not_arms_discrs = Target_imm.Map.keys not_arms in
    if (not (Target_imm.Set.equal arm_discrs Target_imm.all_bools))
      || (not (Target_imm.Set.equal arm_discrs not_arms_discrs))
    then
      None
    else
      Target_imm.Map.data not_arms
      |> List.map Apply_cont.continuation
      |> Continuation.Set.of_list
      |> Continuation.Set.get_singleton
  in
  let create_tagged_scrutinee uacc dest ~make_body =
    (* A problem with using [simplify_let] below is that the continuation
       [dest] might have [Apply_cont_rewrite]s in the environment, left over
       from the simplification of the existing uses.  We must clear these to
       avoid a lookup failure for our new [Apply_cont] when
       [Simplify_apply_cont] tries to rewrite the use.  There is no need for
       the rewrites anyway; they have already been applied.
       Likewise, we need to clear the continuation uses environment for
       [dest] in [dacc], since our new [Apply_cont] might not match the
       original uses (e.g. if a parameter has been removed). *)
    let uacc =
      UA.map_uenv uacc ~f:(fun uenv ->
        UE.delete_apply_cont_rewrite uenv dest)
    in
    let dacc = DA.delete_continuation_uses dacc dest in
    let bound_to = Variable.create "tagged_scrutinee" in
    let body = make_body ~tagged_scrutinee:(Simple.var bound_to) in
    let bound_to = Var_in_binding_pos.create bound_to NM.normal in
    let defining_expr =
      Named.create_prim (Unary (Box_number Untagged_immediate, scrutinee))
        Debuginfo.none
    in
    let let_expr =
      Let.create (Bindable_let_bound.singleton bound_to)
        defining_expr
        ~body
        ~free_names_of_body:Unknown
    in
    simplify_let dacc let_expr
      ~down_to_up:(fun _dacc ~rebuild ->
        rebuild uacc ~after_rebuild:(fun expr uacc -> expr, uacc))
  in
  (* CR mshinwell: Here and elsewhere [UA.name_occurrences] should be empty
     (maybe except for closure vars? -- check).  We should add asserts. *)
  let body, uacc =
    if Target_imm.Map.cardinal arms < 1 then
      let uacc =
        UA.notify_removed ~operation:Removed_operations.branch uacc
      in
      RE.create_invalid (), uacc
    else
      let dbg = Debuginfo.none in
      match switch_is_identity with
      | Some dest ->
        let uacc =
          UA.notify_removed ~operation:Removed_operations.branch uacc
        in
        create_tagged_scrutinee uacc dest ~make_body:(fun ~tagged_scrutinee ->
          (* No need to increment the cost_metrics inside [create_tagged_scrutinee] as it
             will call simplify over the result of [make_body]. *)
          Apply_cont.create dest ~args:[tagged_scrutinee] ~dbg
          |> Expr.create_apply_cont)
      | None ->
        match switch_is_boolean_not with
        | Some dest ->
          let uacc =
            UA.notify_removed ~operation:Removed_operations.branch uacc
          in
          create_tagged_scrutinee uacc dest ~make_body:(fun ~tagged_scrutinee ->
            let not_scrutinee = Variable.create "not_scrutinee" in
            let not_scrutinee' = Simple.var not_scrutinee in
            let do_tagging =
              Named.create_prim (P.Unary (Boolean_not, tagged_scrutinee))
                Debuginfo.none
            in
            let bound =
              VB.create not_scrutinee NM.normal
              |> Bindable_let_bound.singleton
            in
            let body =
              Apply_cont.create dest ~args:[not_scrutinee'] ~dbg
              |> Expr.create_apply_cont
            in
            Let.create bound do_tagging ~body ~free_names_of_body:Unknown
            |> Expr.create_let)
        | None ->
          (* In that case, even though some branches were removed by simplify we
             should not count them in the number of removed operations: these
             branches wouldn't have been taken during execution anyway.
          *)
          let expr, uacc = EB.create_switch uacc ~scrutinee ~arms in
          if !Clflags.flambda_invariant_checks
            && Simple.is_const scrutinee
            && Target_imm.Map.cardinal arms > 1
          then begin
            Misc.fatal_errorf "[Switch] with constant scrutinee (type: %a) \
                should have been simplified away:@ %a"
              T.print scrutinee_ty
              (RE.print (UA.are_rebuilding_terms uacc)) expr
          end;
          expr, uacc
  in
  let uacc, expr =
    ListLabels.fold_left new_let_conts ~init:(uacc, body)
      ~f:(fun (uacc, body)
              (new_cont, new_handler, free_names_of_handler,
               cost_metrics_of_handler) ->
        let free_names_of_body = UA.name_occurrences uacc in
        let expr =
          RE.create_non_recursive_let_cont (UA.are_rebuilding_terms uacc)
            new_cont new_handler ~body ~free_names_of_body
        in
        let name_occurrences =
          Name_occurrences.remove_continuation
            (Name_occurrences.union free_names_of_body free_names_of_handler)
            new_cont
        in
        let uacc =
          UA.with_name_occurrences uacc ~name_occurrences
          |> UA.add_cost_metrics
            (Cost_metrics.increase_due_to_let_cont_non_recursive
               ~cost_metrics_of_handler)
        in
        uacc, expr)
  in
  after_rebuild expr uacc

let simplify_switch ~simplify_let dacc switch ~down_to_up =
  let module AC = Apply_cont in
  let scrutinee = Switch.scrutinee switch in
  let scrutinee_ty =
    S.simplify_simple dacc scrutinee ~min_name_mode:NM.normal
  in
  let scrutinee = T.get_alias_exn scrutinee_ty in
  let arms, dacc =
    let typing_env_at_use = DA.typing_env dacc in
    Target_imm.Map.fold (fun arm action (arms, dacc) ->
        let shape =
          let imm = Target_imm.int (Target_imm.to_targetint arm) in
          T.this_naked_immediate imm
        in
        match T.meet typing_env_at_use scrutinee_ty shape with
        | Bottom -> arms, dacc
        | Ok (_meet_ty, env_extension) ->
          let env_at_use =
            TE.add_env_extension typing_env_at_use env_extension
            |> DE.with_typing_env (DA.denv dacc)
          in
          let args = AC.args action in
          match args with
          | [] ->
            let dacc, rewrite_id =
              DA.record_continuation_use dacc (AC.continuation action)
                Non_inlinable ~env_at_use ~arg_types:[]
            in
            let arms = Target_imm.Map.add arm (action, rewrite_id, []) arms in
            arms, dacc
          | _::_ ->
            let { S. simples = args; simple_tys = arg_types; } =
              S.simplify_simples dacc args
            in
            let dacc, rewrite_id =
              DA.record_continuation_use dacc (AC.continuation action)
                Non_inlinable ~env_at_use ~arg_types
            in
            let arity = List.map T.kind arg_types in
            let action = Apply_cont.update_args action ~args in
            let arms =
              Target_imm.Map.add arm (action, rewrite_id, arity) arms
            in
            arms, dacc)
      (Switch.arms switch)
      (Target_imm.Map.empty, dacc)
    in
    down_to_up dacc
      ~rebuild:(rebuild_switch ~simplify_let dacc ~arms ~scrutinee
        ~scrutinee_ty)

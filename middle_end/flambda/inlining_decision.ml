(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42-66"]
open! Int_replace_polymorphic_compare

module A = Simple_value_approx
module E = Inline_and_simplify_aux.Env
module R = Inline_and_simplify_aux.Result
module W = Inlining_cost.Whether_sufficient_benefit
module T = Inlining_cost.Threshold
module S = Inlining_stats_types
module D = S.Decision
module InliningArgs = Flambda.InliningArgs

let get_function_body (function_decl : A.function_declaration) =
  match function_decl.function_body with
  | None -> assert false
  | Some function_body -> function_body

type ('a, 'b) inlining_result =
  | Changed of (Flambda.t * R.t) * 'a
  | Original of 'b

type 'b good_idea =
  | Try_it
  | Don't_try_it of 'b

type inlining_policy =
  | Always_inline
  | Start_unrolling of { to_depth : int }
  | Continue_unrolling
  | Never_inline
  | Default_inline

let equal_inlining_policy policy1 policy2 =
  match policy1, policy2 with
  | Always_inline, Always_inline -> true
  | Continue_unrolling, Continue_unrolling -> true
  | Never_inline, Never_inline -> true
  | Default_inline, Default_inline -> true
  | Start_unrolling d1, Start_unrolling d2 -> d1.to_depth = d2.to_depth
  | (Always_inline | Continue_unrolling | Never_inline | Default_inline
      | Start_unrolling _), _ ->
      false

let inlining_policy
      ~(func_annot : Lambda.inline_attribute)
      ~(call_annot : Lambda.inline_attribute)
      ~(rec_info : Flambda.rec_info)
    : inlining_policy =
  match rec_info with
  | { depth; unroll_to } when depth < unroll_to ->
    Continue_unrolling
  | _ ->
    (* Merge call site annotation and function annotation.
       The call site annotation takes precedence *)
    begin
      match call_annot with
      | Always_inline when rec_info.depth <= 1 ->
        Always_inline
      | Unroll count when rec_info.depth <= 1 ->
        if count > 0
        then Start_unrolling { to_depth = count }
        else Never_inline
      | Never_inline ->
        Never_inline
      | _ ->
        begin
          match func_annot with
          | Always_inline when rec_info.depth = 0 ->
            Always_inline
          | Unroll count when rec_info.depth = 0 ->
            if count > 0
            then Start_unrolling { to_depth = count }
            else Never_inline
          | Never_inline ->
            Never_inline
          | _ ->
            Default_inline
        end
    end

let inline env r ~lhs_of_application
    ~closure_id_being_applied
    ~(function_decls : A.function_declarations)
    ~(function_body : A.function_body)
    ~value_set_of_closures ~only_use_of_function ~original
    ~(args : Variable.t list) ~size_from_approximation ~dbg ~simplify
    ~(inline_requested : Lambda.inline_attribute)
    ~(specialise_requested : Lambda.specialise_attribute)
    ~max_inlining_arguments
    ~(rec_info : Flambda.rec_info)
    ~fun_cost ~inlining_threshold =
  let toplevel = E.at_toplevel env in
  let branch_depth = E.branch_depth env in
  let policy =
    inlining_policy
      ~func_annot:(function_body.inline)
      ~call_annot:inline_requested
      ~rec_info
  in
  let always_inline =
    match policy with
    | Always_inline | Start_unrolling _ | Continue_unrolling -> true
    | Never_inline | Default_inline -> false
  in
  let unroll_to =
    match policy with
    | Start_unrolling { to_depth = depth } -> depth
    | _ -> 0
  in
  let unrolling_limit =
    let args = E.get_inlining_arguments env in
    (InliningArgs.extract args).inline_max_unroll
  in
  let remaining_inlining_threshold : Inlining_cost.Threshold.t =
    if always_inline then inlining_threshold
    else Lazy.force fun_cost
  in
  let try_inlining =
    if equal_inlining_policy policy Continue_unrolling then
      Try_it
    else if not (E.inlining_allowed env) then
      Don't_try_it S.Not_inlined.Inlining_depth_exceeded
    else if only_use_of_function || always_inline then
      Try_it
    else if equal_inlining_policy policy Never_inline then
      Don't_try_it S.Not_inlined.Annotation
    else if rec_info.depth >= unrolling_limit && function_body.recursive then
      Don't_try_it S.Not_inlined.Unrolling_depth_exceeded
    else if T.equal remaining_inlining_threshold T.Never_inline then
      let threshold =
        match inlining_threshold with
        | T.Never_inline -> assert false
        | T.Can_inline_if_no_larger_than threshold -> threshold
      in
      Don't_try_it (S.Not_inlined.Above_threshold threshold)
    else if not (toplevel && branch_depth = 0)
         && A.all_not_useful (E.find_list_exn env args) then begin
      (* When all of the arguments to the function being inlined are unknown,
         then we cannot materially simplify the function.  As such, we know
         what the benefit of inlining it would be: just removing the call.
         In this case we may be able to prove the function cannot be inlined
         without traversing its body.
         Note that if the function is sufficiently small, we still have to call
         [simplify], because the body needs freshening before substitution.
      *)
      (* CR-someday mshinwell: (from GPR#8): pchambart writes:

          We may need to think a bit about that. I can't see a lot of
          meaningful examples right now, but there are some cases where some
          optimization can happen even if we don't know anything about the
          shape of the arguments.

          For instance

          let f x y = x

          let g x =
            let y = (x,x) in
            f x y
          let f x y =
            if x = y then ... else ...

          let g x = f x x
      *)
      match size_from_approximation with
      | Some body_size ->
        let wsb =
          let benefit = Inlining_cost.Benefit.zero in
          let benefit = Inlining_cost.Benefit.remove_call benefit in
          let benefit =
            Variable.Set.fold (fun v acc ->
                try
                  let t =
                    Var_within_closure.Map.find (Var_within_closure.wrap v)
                      value_set_of_closures.A.bound_vars
                  in
                  match t.A.var with
                  | Some v ->
                    if (E.mem env v) then Inlining_cost.Benefit.remove_prim acc
                    else acc
                  | None -> acc
                with Not_found -> acc)
              function_body.free_variables benefit
          in
          W.create_estimate
            ~original_size:Inlining_cost.direct_call_size
            ~new_size:body_size
            ~toplevel:(E.at_toplevel env)
            ~branch_depth:(E.branch_depth env)
            ~lifting:function_body.A.is_a_functor
            ~args:(E.get_inlining_arguments env)
            ~benefit
        in
        if (not (W.evaluate wsb)) then begin
          Don't_try_it
            (S.Not_inlined.Without_subfunctions wsb)
        end else Try_it
      | None ->
        (* The function is definitely too large to inline given that we don't
           have any approximations for its arguments.  Further, the body
           should already have been simplified (inside its declaration), so
           we also expect no gain from the code below that permits inlining
           inside the body. *)
        Don't_try_it S.Not_inlined.No_useful_approximations
    end else begin
      (* There are useful approximations, so we should simplify. *)
      Try_it
    end
  in
  match try_inlining with
  | Don't_try_it decision -> Original decision
  | Try_it ->
    let r =
      R.set_inlining_threshold r (Some remaining_inlining_threshold)
    in
    let body, r_inlined =
      (* First we construct the code that would result from copying the body of
         the function, without doing any further inlining upon it, to the call
         site. *)
      Inlining_transforms.inline_by_copying_function_body ~env
        ~r:(R.reset_benefit r) ~lhs_of_application ~unroll_to
        ~closure_id_being_applied ~specialise_requested ~inline_requested
        ~function_decls ~function_body ~args ~dbg ~simplify
        ~max_inlining_arguments
    in
    let num_direct_applications_seen =
      (R.num_direct_applications r_inlined) - (R.num_direct_applications r)
    in
    assert (num_direct_applications_seen >= 0);
    let keep_inlined_version decision =
      (* Inlining the body of the function was sufficiently beneficial that we
         will keep it, replacing the call site.  We continue by allowing
         further inlining within the inlined copy of the body. *)
      let r_inlined =
        (* The meaning of requesting inlining is that the user ensure
           that the function has a benefit of at least its size. It is not
           added to the benefit exposed by the inlining because the user should
           have taken that into account before annotating the function. *)
        if always_inline then
          R.map_benefit r_inlined
            (Inlining_cost.Benefit.max ~args:(E.get_inlining_arguments env)
               Inlining_cost.Benefit.(requested_inline ~size_of:body zero))
        else r_inlined
      in
      let r =
        R.map_benefit r_inlined (Inlining_cost.Benefit.(+) (R.benefit r))
      in
      let env = E.note_entering_inlined env in
      let env = E.inside_inlined_function env in
      let env =
        if E.speculation_depth env = 0
           (* If the function was considered for inlining without considering
              its sub-functions, and it is not below another inlining choice,
              then we are certain that this code will be kept. *)
        then env
        else E.speculation_depth_up env
      in
      Changed ((simplify env r body), decision)
    in
    if always_inline then
      keep_inlined_version S.Inlined.Annotation
    else if only_use_of_function then
      keep_inlined_version S.Inlined.Decl_local_to_application
    else begin
      let wsb =
        W.create ~original body
          ~toplevel:(E.at_toplevel env)
          ~branch_depth:(E.branch_depth env)
          ~lifting:function_body.is_a_functor
          ~args:(E.get_inlining_arguments env)
          ~benefit:(R.benefit r_inlined)
      in
      if W.evaluate wsb then
        keep_inlined_version (S.Inlined.Without_subfunctions wsb)
      else if num_direct_applications_seen < 1 then begin
      (* Inlining the body of the function did not appear sufficiently
         beneficial; however, it may become so if we inline within the body
         first.  We try that next, unless it is known that there were
         no direct applications in the simplified body computed above, meaning
         no opportunities for inlining. *)
        Original (S.Not_inlined.Without_subfunctions wsb)
      end else begin
        let env = E.speculation_depth_up env in
        let env = E.note_entering_inlined env in
        let body, r_inlined = simplify env r_inlined body in
        let wsb_with_subfunctions =
          W.create ~original body
            ~toplevel:(E.at_toplevel env)
            ~branch_depth:(E.branch_depth env)
            ~lifting:function_body.is_a_functor
            ~args:(E.get_inlining_arguments env)
            ~benefit:(R.benefit r_inlined)
        in
        if W.evaluate wsb_with_subfunctions then begin
          let decision =
            S.Inlined.With_subfunctions (wsb, wsb_with_subfunctions)
          in
          keep_inlined_version decision
        end
        else begin
          (* r_inlined contains an approximation that may be invalid for the
             untransformed expression: it may reference functions that only
             exists if the body of the function is in fact inlined.
             If the function approximation contained an approximation that
             does not depend on the actual values of its arguments, it
             could be returned instead of [A.value_unknown]. *)
          let decision =
            S.Not_inlined.With_subfunctions (wsb, wsb_with_subfunctions)
          in
          Original decision
        end
      end
    end

let specialise env r ~lhs_of_application
      ~(function_decls : A.function_declarations)
      ~(function_decl : A.function_declaration)
      ~(function_body : A.function_body)
      ~closure_id_being_applied
      ~(value_set_of_closures : A.value_set_of_closures)
      ~args ~args_approxs ~dbg ~simplify ~original ~rec_info
      ~inlining_threshold ~fun_cost
      ~inline_requested ~specialise_requested
      ~max_inlining_arguments =
  let invariant_params = value_set_of_closures.invariant_params in
  let free_vars = value_set_of_closures.free_vars in
  let has_no_useful_approxes =
    lazy
      (List.for_all2
         (fun id approx ->
            not ((A.useful approx)
                 && Variable.Map.mem id (Lazy.force invariant_params)))
         (Parameter.List.vars function_decl.params) args_approxs)
  in
  let always_specialise, never_specialise =
    (* Merge call site annotation and function annotation.
       The call site annotation takes precedence *)
    match (specialise_requested : Lambda.specialise_attribute) with
    | Always_specialise -> true, false
    | Never_specialise -> false, true
    | Default_specialise -> begin
        match function_decl.function_body with
        | None -> false, true
        | Some { specialise } ->
          match (specialise : Lambda.specialise_attribute) with
          | Always_specialise -> true, false
          | Never_specialise -> false, true
          | Default_specialise -> false, false
      end
  in
  let remaining_inlining_threshold : Inlining_cost.Threshold.t =
    if always_specialise then inlining_threshold
    else Lazy.force fun_cost
  in
  let try_specialising =
    (* Try specialising if the function:
       - is recursive; and
       - is closed (it and all other members of the set of closures on which
         it depends); and
       - has useful approximations for some invariant parameters. *)
    if Flambda.is_classic_mode_on function_decls.is_classic_mode then
      Don't_try_it S.Not_specialised.Classic_mode
    else if always_specialise && not (Lazy.force has_no_useful_approxes) then
      Try_it
    else if never_specialise then
      Don't_try_it S.Not_specialised.Annotation
    else if T.equal remaining_inlining_threshold T.Never_inline then
      let threshold =
        match inlining_threshold with
        | T.Never_inline -> assert false
        | T.Can_inline_if_no_larger_than threshold -> threshold
      in
      Don't_try_it (S.Not_specialised.Above_threshold threshold)
    else if not (Variable.Map.is_empty free_vars) then
      Don't_try_it S.Not_specialised.Not_closed
    else if not function_body.recursive then
      Don't_try_it S.Not_specialised.Not_recursive
    else if Variable.Map.is_empty (Lazy.force invariant_params) then
      Don't_try_it S.Not_specialised.No_invariant_parameters
    else if Lazy.force has_no_useful_approxes then
      Don't_try_it S.Not_specialised.No_useful_approximations
    else Try_it
  in
  match try_specialising with
  | Don't_try_it decision -> Original decision
  | Try_it -> begin
      let r =
        R.set_inlining_threshold r (Some remaining_inlining_threshold)
      in
      let copied_function_declaration =
        Inlining_transforms.inline_by_copying_function_declaration ~env
          ~r:(R.reset_benefit r) ~lhs_of_application ~rec_info
          ~function_decls ~function_decl ~closure_id_being_applied
          ~args ~args_approxs
          ~invariant_params:invariant_params
          ~specialised_args:value_set_of_closures.specialised_args
          ~free_vars:value_set_of_closures.free_vars
          ~direct_call_surrogates:value_set_of_closures.direct_call_surrogates
          ~dbg ~simplify ~inline_requested ~max_inlining_arguments
      in
      match copied_function_declaration with
      | Some (expr, r_inlined) ->
        let wsb =
          W.create ~original expr
            ~toplevel:false
            ~branch_depth:(E.branch_depth env)
            ~lifting:false
            ~args:(E.get_inlining_arguments env)
            ~benefit:(R.benefit r_inlined)
        in
        let env =
          (* CR-someday lwhite: could avoid calculating this if stats is turned
             off *)
          let closure_ids =
            Closure_id.Set.of_list (
              List.map Closure_id.wrap
                (Variable.Set.elements (Variable.Map.keys function_decls.funs)))
          in
          E.note_entering_specialised env ~closure_ids
        in let keep_specialised decision =
          let r_inlined =
            if always_specialise then
              R.map_benefit r_inlined
                (Inlining_cost.Benefit.max ~args:(E.get_inlining_arguments env)
                   Inlining_cost.Benefit.(requested_inline ~size_of:expr zero))
            else r_inlined
          in
          let r =
            R.map_benefit r_inlined (Inlining_cost.Benefit.(+) (R.benefit r))
          in
          let closure_env =
            let env =
              if E.speculation_depth env = 0
               (* If the function was considered for specialising without
                  considering its sub-functions, and it is not below another
                  inlining choice, then we are certain that this code will
                  be kept. *)
              then env
              else E.speculation_depth_up env
            in
              E.set_never_inline_outside_closures env
          in
          let expr, r = simplify closure_env r expr in
          let application_env = E.set_never_inline_inside_closures env in
          (* Inlining depths just updated above; don't update them again! *)
          let application_env = E.clear_inlining_depth application_env in
          let res = simplify application_env r expr in
          Changed (res, decision)
        in
        if always_specialise then
          keep_specialised S.Specialised.Annotation
        else if W.evaluate wsb then
          keep_specialised (S.Specialised.Without_subfunctions wsb)
        else begin
          let closure_env =
            let env = E.speculation_depth_up env in
            E.set_never_inline_outside_closures env
          in
          let expr, r_inlined = simplify closure_env r_inlined expr in
          let wsb_with_subfunctions =
            W.create ~original expr
              ~toplevel:false
              ~branch_depth:(E.branch_depth env)
              ~lifting:false
              ~args:(E.get_inlining_arguments env)
              ~benefit:(R.benefit r_inlined)
          in
          if W.evaluate wsb_with_subfunctions then begin
            keep_specialised
              (S.Specialised.With_subfunctions (wsb, wsb_with_subfunctions))
          end else begin
            let decision =
              S.Not_specialised.Not_beneficial (wsb, wsb_with_subfunctions)
            in
            Original decision
          end
        end
      | None ->
        let decision = S.Not_specialised.No_useful_approximations in
        Original decision
    end

let for_call_site ~env ~r ~(function_decls : A.function_declarations)
      ~lhs_of_application ~(rec_info : Flambda.rec_info)
      ~closure_id_being_applied
      ~(function_decl : A.function_declaration)
      ~(value_set_of_closures : A.value_set_of_closures)
      ~args ~args_approxs ~dbg ~simplify ~inline_requested
      ~specialise_requested ~max_inlining_arguments =
  if List.length args <> List.length args_approxs then begin
    Misc.fatal_error "Inlining_decision.for_call_site: inconsistent lengths \
        of [args] and [args_approxs]"
  end;
  let inlining_arguments = E.get_inlining_arguments env in
  let original =
    Flambda.Apply {
      func = lhs_of_application;
      args;
      kind = Direct closure_id_being_applied;
      dbg;
      inlining_depth = E.inlining_depth env;
      inline = inline_requested;
      specialise = specialise_requested;
      max_inlining_arguments = max_inlining_arguments;
    }
  in
  let original_r =
    R.set_approx (R.seen_direct_application r) (A.value_unknown Other)
  in
  match function_decl.function_body with
  | None -> original, original_r
  | Some function_body ->
    if function_body.stub then begin
      let function_body = get_function_body function_decl in
      let body, r =
        Inlining_transforms.inline_by_copying_function_body ~env
          ~unroll_to:0 ~r ~lhs_of_application
          ~closure_id_being_applied ~specialise_requested ~inline_requested
          ~function_decls ~function_body ~args ~dbg ~simplify
          ~max_inlining_arguments
      in
      simplify env r body
    end else if E.never_inline env then
      (* This case only occurs when examining the body of a stub function
         but not in the context of inlining said function.  As such, there
         is nothing to do here (and no decision to report). *)
      original, original_r
    else if Flambda.is_classic_mode_on function_decls.is_classic_mode then begin
      let env =
        E.note_entering_call env
          ~closure_id:closure_id_being_applied ~dbg:dbg
      in
      let simpl =
        match function_decl.function_body with
        | None -> Original S.Not_inlined.Classic_mode
        | Some function_body ->
          let try_inlining =
            if rec_info.depth >= 1 then
              Don't_try_it S.Not_inlined.Unrolling_depth_exceeded
            else if not (E.inlining_allowed env) then
              Don't_try_it S.Not_inlined.Inlining_depth_exceeded
            else
                Try_it
          in
          match try_inlining with
          | Don't_try_it decision -> Original decision
          | Try_it ->
            let body, r =
              Inlining_transforms.inline_by_copying_function_body ~env
                ~unroll_to:0 ~r ~function_body ~lhs_of_application
                ~closure_id_being_applied ~specialise_requested ~inline_requested
                ~function_decls ~args ~dbg ~simplify ~max_inlining_arguments
            in
            let env = E.note_entering_inlined env in
            let env = E.inside_inlined_function env in
            Changed ((simplify env r body), S.Inlined.Classic_mode)
      in
      let res, decision =
        match simpl with
        | Original decision ->
          let decision =
            S.Decision.Unchanged (S.Not_specialised.Classic_mode, decision)
          in
          (original, original_r), decision
        | Changed ((expr, r), decision) ->
          let max_inlining_threshold =
            if E.at_toplevel env then
              Inline_and_simplify_aux.initial_inlining_toplevel_threshold
                inlining_arguments
            else
              Inline_and_simplify_aux.initial_inlining_threshold
                inlining_arguments
          in
          let raw_inlining_threshold = R.inlining_threshold r in
          let unthrottled_inlining_threshold =
            match raw_inlining_threshold with
            | None -> max_inlining_threshold
            | Some inlining_threshold -> inlining_threshold
          in
          let inlining_threshold =
            T.min unthrottled_inlining_threshold max_inlining_threshold
          in
          let inlining_threshold_diff =
            T.sub unthrottled_inlining_threshold inlining_threshold
          in
          let res =
            if E.speculation_depth env = 0
            then expr, R.set_inlining_threshold r raw_inlining_threshold
            else expr, R.add_inlining_threshold r inlining_threshold_diff
          in
          res, S.Decision.Inlined (S.Not_specialised.Classic_mode, decision)
      in
      E.record_decision env decision;
      res
    end else begin
      let function_body = get_function_body function_decl in
      let env = E.unset_never_inline_inside_closures env in
      let env =
        E.note_entering_call env
          ~closure_id:closure_id_being_applied ~dbg:dbg
      in
      let max_level = (InliningArgs.extract inlining_arguments).inline_max_speculation_depth
      in
      let raw_inlining_threshold = R.inlining_threshold r in
      let max_inlining_threshold =
        if E.at_toplevel env then
          Inline_and_simplify_aux.initial_inlining_toplevel_threshold inlining_arguments
        else
          Inline_and_simplify_aux.initial_inlining_threshold
            inlining_arguments
      in
      let unthrottled_inlining_threshold =
        match raw_inlining_threshold with
        | None -> max_inlining_threshold
        | Some inlining_threshold -> inlining_threshold
      in
      let inlining_threshold =
        T.min unthrottled_inlining_threshold max_inlining_threshold
      in
      let inlining_threshold_diff =
        T.sub unthrottled_inlining_threshold inlining_threshold
      in
      let inlining_prevented =
        match inlining_threshold with
        | Never_inline -> true
        | Can_inline_if_no_larger_than _ -> false
      in
      let simpl =
        if inlining_prevented then
          Original (D.Prevented Function_prevented_from_inlining)
        else if E.speculation_depth env >= max_level then
          Original (D.Prevented Level_exceeded)
        else begin
          let fun_cost =
            lazy
              (Inlining_cost.can_try_inlining function_body.body
                 inlining_threshold
                 ~number_of_arguments:(List.length function_decl.params)
                 (* CR-someday mshinwell: for the moment, this is None, since
                    the Inlining_cost code isn't checking sizes up to the max
                    inlining threshold---this seems to take too long. *)
                 ~size_from_approximation:None)
          in
          let specialise_result =
            specialise env r
              ~function_decls ~function_decl ~function_body
              ~lhs_of_application ~closure_id_being_applied
              ~value_set_of_closures ~args ~args_approxs ~dbg ~simplify
              ~original ~inline_requested ~specialise_requested ~fun_cost
              ~rec_info ~inlining_threshold ~max_inlining_arguments
          in
          match specialise_result with
          | Changed (res, spec_reason) ->
            Changed (res, D.Specialised spec_reason)
          | Original spec_reason ->
            let only_use_of_function = false in
            (* If we didn't specialise then try inlining *)
            let size_from_approximation =
              let fun_var = Closure_id.unwrap closure_id_being_applied in
              match
                Variable.Map.find fun_var
                                  (Lazy.force value_set_of_closures.size)
              with
              | size -> size
              | exception Not_found ->
                Misc.fatal_errorf "Approximation does not give a size for the \
                                   function having fun_var %a.  \
                                   value_set_of_closures: %a"
                  Variable.print fun_var
                  A.print_value_set_of_closures value_set_of_closures
            in
            let inline_result =
              inline env r ~lhs_of_application
                ~closure_id_being_applied ~function_decls ~value_set_of_closures
                ~only_use_of_function ~original
                ~inline_requested ~specialise_requested ~args
                ~size_from_approximation ~dbg ~simplify ~fun_cost ~rec_info
                ~inlining_threshold ~function_body ~max_inlining_arguments
            in
            match inline_result with
            | Changed (res, inl_reason) ->
              Changed (res, D.Inlined (spec_reason, inl_reason))
            | Original inl_reason ->
              Original (D.Unchanged (spec_reason, inl_reason))
        end
      in
      let res, decision =
        match simpl with
        | Original decision -> (original, original_r), decision
        | Changed ((expr, r), decision) ->
          let res =
            if E.speculation_depth env = 0
            then expr, R.set_inlining_threshold r raw_inlining_threshold
            else expr, R.add_inlining_threshold r inlining_threshold_diff
          in
          res, decision
      in
      E.record_decision env decision;
      res
    end

(* We do not inline inside stubs, which are always inlined at their call site.
   Inlining inside the declaration of a stub could result in more code than
   expected being inlined (e.g. the body of a function that was transformed
   by adding the stub). *)
let should_inline_inside_declaration (decl : Flambda.function_declaration) =
  not decl.stub

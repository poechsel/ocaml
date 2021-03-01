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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

module DA = Downwards_acc
module DE = Downwards_env
module I = Flambda_type.Function_declaration_type.Inlinable
module VB = Var_in_binding_pos

let make_inlined_body ~callee ~unroll_to ~params ~args ~my_closure ~body
      ~exn_continuation ~return_continuation ~apply_exn_continuation
      ~apply_return_continuation =
  let perm = Name_permutation.empty in
  let perm =
    match (apply_return_continuation : Apply.Result_continuation.t)with
    | Return k ->
       Name_permutation.add_continuation perm
         return_continuation k
    | Never_returns -> perm
  in
  let perm =
    Name_permutation.add_continuation perm
      (Exn_continuation.exn_handler exn_continuation)
      apply_exn_continuation
  in
  let callee =
    Simple.merge_rec_info callee
      ~newer_rec_info:(Some (Rec_info.create ~depth:1 ~unroll_to))
    |> Option.get  (* CR mshinwell: improve *)
  in
  Expr.apply_name_permutation
    (Expr.bind_parameters_to_args_no_simplification
       ~params ~args
       ~body:(Let.create
                (Bindable_let_bound.singleton
                   (VB.create my_closure Name_mode.normal))
                (Named.create_simple callee)
                ~body
                (* Here and below, we don't need to give any name occurrence
                      information (thank goodness!) since the entirety of the
                      expression we're building will be re-simplified. *)
                ~free_names_of_body:Unknown
              |> Expr.create_let))
    perm

let wrap_inlined_body_for_exn_support ~extra_args ~apply_exn_continuation
      ~apply_return_continuation ~result_arity ~make_inlined_body =
  (* We need to add a wrapper for the exception handler, so that
     exceptions coming from the inlined body go through the wrapper
     and are re-raised with the correct extra arguments.
     This means we also need to add a push trap before the inlined
     body, and a pop trap after.
     The push trap is simply a matter of jumping to the body, while
     the pop trap needs to replace the body's return continuation
     with a wrapper that pops then jumps back.

     As a result, the application [Apply_expr f (args) <k> «k_exn»]
     is replaced (before the actual inlining) by:
     [let_cont_exn k1 (exn: val) =
       Apply_cont k_exn exn extra_args
     in
     let_cont k_pop (args) = Apply_cont<pop k1> k args in
     let_cont k_push () = Apply_expr f (args) <k_pop> «k1» in
     Apply_cont<push k1> k_push ()]

     This feels quite heavy, but is necessary because we can rewrite
     neither the definition and other uses of k_exn nor the
     uses of the exception continuation in the body of f, so we need
     two distinct exception continuations; and of course the new
     exception continuation needs to be correctly pushed and popped.

     The most annoying part of this is that it introduces trywith
     blocks that were not part of the initial program, will not be
     removed, and might be useless (if the function never raises).

     Maybe a better solution would be to propagate through dacc
     a lazy rewriting, that would add the correct extra args
     to all uses of the exception continuation in the body.
   *)
  let wrapper = Continuation.create ~sort:Exn () in
  let body_with_pop =
    match (apply_return_continuation : Apply.Result_continuation.t) with
    | Never_returns ->
       make_inlined_body ~apply_exn_continuation:wrapper
         ~apply_return_continuation
    | Return apply_return_continuation ->
       let pop_wrapper_cont = Continuation.create () in
       let pop_wrapper_handler =
         let kinded_params =
           List.map (fun k -> Variable.create "wrapper_return", k)
             result_arity
         in
         let trap_action =
           Trap_action.Pop { exn_handler = wrapper; raise_kind = None; }
         in
         let args = List.map (fun (v, _) -> Simple.var v) kinded_params in
         let apply_cont =
           Apply_cont.create ~trap_action
             apply_return_continuation
             ~args
             ~dbg:Debuginfo.none
         in
         let handler = Expr.create_apply_cont apply_cont in
         Continuation_handler.create
           (Kinded_parameter.List.create kinded_params)
           ~handler
           ~free_names_of_handler:Unknown
           ~is_exn_handler:false
       in
       let new_apply_return_continuation =
         Apply.Result_continuation.Return pop_wrapper_cont
       in
       let body =
         make_inlined_body ~apply_exn_continuation:wrapper
           ~apply_return_continuation:new_apply_return_continuation
       in
       Let_cont.create_non_recursive pop_wrapper_cont
         pop_wrapper_handler ~body
         ~free_names_of_body:Unknown
  in
  let wrapper_handler =
    let param = Variable.create "exn" in
    let kinded_params =
      [KP.create param K.With_subkind.any_value]
    in
    let exn_handler =
      Exn_continuation.exn_handler apply_exn_continuation
    in
    let trap_action =
      Trap_action.Pop { exn_handler; raise_kind = None; }
    in
    let handler =
      Expr.create_apply_cont (Apply_cont.create
                                ~trap_action
                                (Exn_continuation.exn_handler apply_exn_continuation)
                                ~args:((Simple.var param) :: (List.map fst extra_args))
                                ~dbg:Debuginfo.none) (* Backtrace building functions expect
                                            compiler-generated raises not to
                                            have any debug info *)
    in
    Continuation_handler.create kinded_params ~handler
      ~free_names_of_handler:Unknown
      ~is_exn_handler:true
  in
  let body_with_push =
    (* Wrap the body between push and pop of the wrapper handler *)
    let push_wrapper_cont = Continuation.create () in
    let handler = body_with_pop in
    let push_wrapper_handler =
      Continuation_handler.create [] ~handler
        ~free_names_of_handler:Unknown
        ~is_exn_handler:false
    in
    let trap_action =
      Trap_action.Push { exn_handler = wrapper; }
    in
    let body =
      Expr.create_apply_cont (Apply_cont.create
                                ~trap_action
                                push_wrapper_cont
                                ~args: []
                                ~dbg: Debuginfo.none)
    in
    Let_cont.create_non_recursive push_wrapper_cont
      push_wrapper_handler ~body
      ~free_names_of_body:Unknown
  in
  Let_cont.create_non_recursive wrapper wrapper_handler
    ~body:body_with_push
    ~free_names_of_body:Unknown


let inline dacc ~callee ~args function_decl
      ~apply_return_continuation ~apply_exn_continuation
      ~apply_inlining_state ~unroll_to dbg =
  (* CR mshinwell: Add meet constraint to the return continuation *)
  let denv = DA.denv dacc in
  let code = DE.find_code denv (I.code_id function_decl) in
  let params_and_body =
    Code.params_and_body_must_be_present code ~error_context:"Inlining"
  in
  Function_params_and_body.pattern_match params_and_body
    ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure
            ~is_my_closure_used:_ ->
          let denv =
            DE.set_inlining_state
              (DE.set_inlined_debuginfo denv dbg)
              (Inlining_state.increment_depth apply_inlining_state)
          in
          let make_inlined_body =
            make_inlined_body ~callee ~unroll_to ~params ~args ~my_closure ~body
              ~exn_continuation ~return_continuation
          in
          let expr =
            assert (Exn_continuation.extra_args exn_continuation = []);
            match Exn_continuation.extra_args apply_exn_continuation with
            | [] ->
              make_inlined_body ~apply_exn_continuation:
                (Exn_continuation.exn_handler apply_exn_continuation)
                ~apply_return_continuation
            | extra_args ->
               wrap_inlined_body_for_exn_support ~extra_args 
                 ~apply_exn_continuation ~apply_return_continuation
                 ~result_arity:(Code.result_arity code) ~make_inlined_body
          in
(*
  Format.eprintf "Inlined body to be simplified:@ %a\n%!"
    Expr.print expr;
*)
(*
  Format.eprintf "Inlined body to be simplified:@ %a@ dacc:@ %a\n%!"
    Expr.print expr DA.print dacc;
*)
          DA.with_denv dacc denv, expr)

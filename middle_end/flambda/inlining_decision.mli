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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** See the Flambda manual chapter for an explanation in prose of the
    inlining decision procedure. *)


(** Small summary of the inlining procedure

# For_call_site <-- meta-method which is called on every call site

We distinguish the different cases
- If we want to inline a stub, we simplify [meaning we are
    launching inline and simplify again] on its content
- If we set the never inline flag in the env, then we do nothing
- Otherwise, we will try to inline and specialise the call.
    If we don't have enough threshold to inline, or
    if we have reached our maximum speculation depth, then we stop
    the process.
    Otherwise, we first try to specialise the call. If it's not
    successfull, we will try to inline it.


# Inline <-- method used to inline a call

We go through a decision tree to see if we can try inlining:
    - If our policy is to continue unrolling, try inlining
    - If we are not allowed to inline, don't try it
    - If we are in classic mode, don't try it
    - If we should always inline, try it
    - If we should never inline, don't try it
    - If we've exceeded the maximum unrolling depth and are trying
      to unroll, don't try it
    - If we have not enough remaining threshold, don't try it
    - If we don't have any informations on the arguments, a micro
      optimisation is done to decide wether to try inlining or not
    - Otherwise, try inlining
If we decided not to try inlining, quit.
Otherwise, we first get the inlined function. If we are sure we want to
    inline it (because of an annotation, or because the evaluated benefit
    is good enough), then we keep it.
    If it's not the case, we speculate on the inlining. We increase speculation
    depth, and call the whole inlining and simplify procedure on the inlined function.
    If this speculative inlining achieve an improvement, we keep the inlined function.

# Specialise <-- Method used to specialise a call

We go through a decision tree to see if we can try specialising:
    - If we are in classic mode, don't try it
    - If we are not allowed to specialise, don't try it
    - If we are sure to specialise and we have at least one useful approximation
      for a parameter, try it
    - If we never want to specialise, don't try it
    - If we have not enough remaining threshold, don't try it
    - If we don't have any bound variables, don't try it
    - If the function is not recursive, don't try it
    - If we have no invariant parameters, don't try it
    - If we don't have useful approximations of parameters, don't try it
    - Otherwise don't try it
If we decided not to try specialising, quit.
Otherwise, first get the specialised function and the simplified call. If we always
    want to specialise or if it the evaluated benefit of the specialisation is good
    enough, we keep the specialised version.
    Otherwise, we first evaluate speculatively the interest of specialising the
    function (by calling inlining and simplify on the body of the specialised function
    with an increased speculative depth). If it yields an improvement, we keep
    the specialised function.
*)


type call_informations

type callee_informations

type annotations

val build_call_structure :
  callee:Variable.t
  -> args:Variable.t list
  -> dbg:Debuginfo.t
  -> rec_info:Flambda.rec_info
  -> call_informations

val build_callee_structure :
  function_decls:Simple_value_approx.function_declarations
  -> function_decl:Simple_value_approx.function_declaration
  -> closure_id_being_applied:Closure_id.t
  -> value_set_of_closures:Simple_value_approx.value_set_of_closures
  -> callee_informations

val build_annotations_structure :
  caller_inline:Lambda.inline_attribute
  -> caller_specialise:Lambda.specialise_attribute
  -> callee:Simple_value_approx.function_declaration
  -> annotations



(** Try to inline a full application of a known function, guided by various
    heuristics. *)
val for_call_site
   : env:Inline_and_simplify_aux.Env.t
  -> r:Inline_and_simplify_aux.Result.t
  -> call:call_informations
  -> callee:callee_informations
  -> annotations:annotations
  -> args_approxs:Simple_value_approx.t list
  -> simplify:Inlining_decision_intf.simplify
  -> Flambda.t * Inline_and_simplify_aux.Result.t

(** When a function declaration is encountered by [for_call_site], the body
    may be subject to inlining immediately, thus changing the declaration.
    This function must return [true] for that to be able to happen. *)
val should_inline_inside_declaration : Flambda.function_declaration -> bool

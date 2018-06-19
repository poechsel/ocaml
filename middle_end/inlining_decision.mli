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

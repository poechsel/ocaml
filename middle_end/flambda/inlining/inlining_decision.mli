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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module Function_declaration_decision : sig
  type t = private
    | Never_inline_attribute
    | Function_body_too_large of Code_size.t
    | Stub
    | Inline of (Code_size.t * Code_size.t) option

  val print : Format.formatter -> t -> unit

  val report : Format.formatter -> t -> unit

  val can_inline : t -> bool
end


type cost_metrics_source = From_denv | Metrics of Cost_metrics.t

(* If a cost_metrics is passed it will be used as the cost metrics for
   the function declaration instead of grabbing the cost metrics from the
   one stored on the code located in the env. *)
val make_decision_for_function_declaration
   : Downwards_env.t
  -> cost_metrics_source:cost_metrics_source
  -> Function_declaration.t
  -> Function_declaration_decision.t

module Call_site_decision : sig
  type attribute_causing_inlining = private
    | Unroll
    | Always

  type t = private
    | Environment_says_never_inline
    | Unrolling_depth_exceeded
    | Max_inlining_depth_exceeded
    | Recursion_depth_exceeded
    | Never_inline_attribute
    | Rejected_by_cost_metrics
    | Inline of {
        attribute : attribute_causing_inlining option;
        unroll_to : int option;
      }

  val print : Format.formatter -> t -> unit

  val report : Format.formatter -> t -> unit

  type can_inline = private
    | Do_not_inline
    | Inline of { unroll_to : int option; }

  val can_inline : t -> can_inline
end

val make_decision_for_call_site
   : Downwards_acc.t
  -> simplify_expr:Expr.t Simplify_common.expr_simplifier
  -> function_decl:Flambda_type.Function_declaration_type.Inlinable.t
  -> function_decl_rec_info:Rec_info.t
  -> apply:Apply.t
  -> Call_site_decision.t

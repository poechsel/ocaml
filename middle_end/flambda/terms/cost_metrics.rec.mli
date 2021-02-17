
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

module Benefits : sig
  type t

  val zero: t
  val print: Format.formatter -> t -> unit

  val call: t -> t
  val alloc: count:int -> t -> t
  val prim: prim:Flambda_primitive.t -> t -> t
  val branch: count:int -> t -> t
  val direct_call_of_indirect: t -> t
end

type t
(* It's best to avoid calling this function too much as it is
   quite slow. *)
val expr : find_code_cost_metrics:(Code_id.t -> t Or_unknown.t) -> Expr.t -> t

val of_int : int -> t
val to_int : t -> int
val (+) : t -> t -> t
val smaller : t -> than:t -> bool
val equal : t -> t -> bool
val print : Format.formatter -> t -> unit

val prim : Flambda_primitive.t -> t
val simple : Simple.t -> t
val static_consts : Static_const.Group.t -> t
val set_of_closures : find_code_cost_metrics:(Code_id.t -> t Or_unknown.t) -> Set_of_closures.t -> t

val let_expr_don't_consider_body : cost_metrics_of_defining_expr:t -> t
val apply : Apply.t -> t
val apply_cont : Apply_cont.t -> t
val switch : Switch.t -> t
val invalid : unit -> t
val let_cont_non_recursive_don't_consider_body : cost_metrics_of_handler:t -> t
val let_cont_recursive_don't_consider_body : cost_metrics_of_handlers:t -> t

(* Benefits are finer grained metrics (number of calls / allocations) about
   the size of an expression. [positive_benefits], which maps to expression
   that are newly created, are separated from [negative_benefits] which are
   benefits belonging to expression that were omitted while rebuilding a
   term. *)
val positive_benefits : t -> Benefits.t
val negative_benefits : t -> Benefits.t

val remove_call : t -> t
val remove_alloc : t -> t
val remove_prim : prim:Flambda_primitive.t -> t -> t
val remove_branch : count:int -> t -> t
val direct_call_of_indirect: t -> t

val delete_code_track_benefits : positive_benefits:Benefits.t -> t -> t

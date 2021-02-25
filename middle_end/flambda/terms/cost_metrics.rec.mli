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

(** Cost metrics are a group of metrics tracking the impact of simplifying an
    expression. One of these is an approximation of the size of the generated
    machine code for this expression. It also tracks the number of operations
    that should have been executed but were removed by the simplifier.*)

module Operations : sig
  type t

  val call: t
  val branch: t
  val prim: Flambda_primitive.t -> t
  val alloc: t
  val direct_call_of_indirect: t
end

type t

(* It's best to avoid calling this function too much as it is
   quite slow. *)
val expr_size : find_code:(Code_id.t -> Code.t) -> Expr.t -> t

val zero : t
val smaller_than_threshold : t -> threshold:int -> bool
val size : t -> int
val equal_size : t -> t -> bool
val print : Format.formatter -> t -> unit

val prim : Flambda_primitive.t -> t
val simple : Simple.t -> t
val static_consts : Static_const.Group.t -> t
val set_of_closures : find_cost_metrics:(Code_id.t -> t Or_unknown.t) -> Set_of_closures.t -> t

val apply : Apply.t -> t
val apply_cont : Apply_cont.t -> t
val switch : Switch.t -> t
val invalid : t
val increase_due_to_let_expr : is_phantom:bool -> cost_metrics_of_defining_expr:t -> t
val increase_due_to_let_cont_non_recursive : cost_metrics_of_handler:t -> t
val increase_due_to_let_cont_recursive : cost_metrics_of_handlers:t -> t

val add : added:t -> t -> t
val remove_operation : Operations.t -> t -> t

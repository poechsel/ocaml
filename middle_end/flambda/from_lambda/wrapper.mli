open Flambda.Import

module With_size : sig
  type 'a t

  val create : size:Cost_metrics.t -> 'a -> 'a t

  val size : 'a t -> Cost_metrics.t

  val get : 'a t -> 'a
end

module Expr_with_size : sig
  type t = Expr.t With_size.t
  val create_apply_cont : Apply_cont.t -> t
  val create_apply : Apply.t -> t
  val create_let : Let.t With_size.t -> t
  val create_switch : Switch.t -> t
  val create_invalid : ?semantics:Invalid_term_semantics.t -> unit -> t
end

module Named_with_size : sig
  type t = Named.t With_size.t
  val create_simple : Simple.t -> t

  val create_prim : Flambda_primitive.t -> Debuginfo.t -> t

  val create_set_of_closures :
    find_cost_metrics:(Code_id.t -> Cost_metrics.t)
    -> Set_of_closures.t -> t

  val create_static_consts : Static_const.Group.t -> t
end

module Let_with_size : sig
  val create
    : Bindable_let_bound.t
    -> Named_with_size.t
    -> body:Expr_with_size.t
    -> free_names_of_body:Name_occurrences.t Or_unknown.t
    -> Let.t With_size.t
end

module Continuation_handler_with_size : sig
  val create
    : Kinded_parameter.t list
    -> handler:Expr_with_size.t
    -> free_names_of_handler:Name_occurrences.t Or_unknown.t
    -> is_exn_handler:bool
    -> Continuation_handler.t With_size.t
end

module Let_cont_with_size : sig
  val create_non_recursive
    : Continuation.t
    -> Continuation_handler.t With_size.t
    -> body:Expr_with_size.t
    -> free_names_of_body:Name_occurrences.t Or_unknown.t
    -> Expr_with_size.t

  val create_recursive
    : Continuation_handler.t With_size.t Continuation.Map.t
    -> body:Expr_with_size.t
    -> Expr_with_size.t
end

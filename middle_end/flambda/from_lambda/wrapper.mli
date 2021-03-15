open Flambda.Import
open Closure_conversion_aux

module With_size : sig
  type 'a t

  val create : size:Cost_metrics.t -> 'a -> 'a t

  val size : 'a t -> Cost_metrics.t

  val get : 'a t -> 'a
end

module Expr_with_size : sig
  type t = Expr.t
  val create_apply_cont : Acc.t -> Apply_cont.t -> Acc.t * t
  val create_apply : Acc.t -> Apply.t -> Acc.t * t
  val create_let : Acc.t * Let.t -> Acc.t * t
  val create_switch : Acc.t -> Switch.t -> Acc.t * t
  val create_invalid :
    Acc.t
    -> ?semantics:Invalid_term_semantics.t
    -> unit
    -> Acc.t * t
end

module Let_with_size : sig
  val create
    : Acc.t
    -> Bindable_let_bound.t
    -> Named.t
    -> body:Expr_with_size.t
    -> free_names_of_body:Name_occurrences.t Or_unknown.t
    -> Acc.t * Let.t 
end

module Continuation_handler_with_size : sig
  val create
    : Acc.t
    -> Kinded_parameter.t list
    -> handler:Expr_with_size.t
    -> free_names_of_handler:Name_occurrences.t Or_unknown.t
    -> is_exn_handler:bool
    -> Acc.t * Continuation_handler.t
end

module Let_cont_with_size : sig
  val create_non_recursive
    : Acc.t
    -> Continuation.t
    -> Continuation_handler.t
    -> body:Expr_with_size.t
    -> free_names_of_body:Name_occurrences.t Or_unknown.t
    -> cost_metrics_of_handler:Cost_metrics.t
    -> Acc.t * Expr_with_size.t

  val create_recursive
    : Acc.t
    -> Continuation_handler.t Continuation.Map.t
    -> body:Expr_with_size.t
    -> cost_metrics_of_handlers:Cost_metrics.t
    -> Acc.t * Expr_with_size.t
end

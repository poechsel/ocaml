include Flambda.Import

open Closure_conversion_aux

module Expr_wrapper = struct
  type t = Expr.t

  let create_apply_cont acc apply_cont =
    let acc =
      Acc.increment_metrics
        (Code_size.apply_cont apply_cont |> Cost_metrics.from_size)
        acc
    in
    acc, Expr.create_apply_cont apply_cont

  let create_apply acc apply =
    let acc =
      Acc.increment_metrics
        (Code_size.apply apply |> Cost_metrics.from_size)
        acc
    in
    acc, Expr.create_apply apply

  let create_let (acc, let_expr) =
    (* The signature for create_let is a bit different. It is mainly used to
       materialize and expression coming from Let_cont_wrapper where the cost metrics
       was already computed. The signature is made so that the result from
       Let_cont_wrapper can be directly piped through [create_let].*)
    acc, Expr.create_let let_expr

  let create_switch acc switch =
    let acc =
      Acc.increment_metrics
        (Code_size.switch switch |> Cost_metrics.from_size)
        acc
    in
    acc, Expr.create_switch switch

  let create_invalid acc ?semantics () =
    let acc =
      Acc.increment_metrics
        (Code_size.invalid |> Cost_metrics.from_size)
        acc
    in
    acc, Expr.create_invalid ?semantics ()
end

module Let_wrapper = struct
  let create acc let_bound named ~body ~free_names_of_body =
    let cost_metrics_of_defining_expr =
      match named with
      | Named.Prim (prim, _) ->
        Code_size.prim prim |> Cost_metrics.from_size
      | Named.Simple simple ->
        Code_size.simple simple |> Cost_metrics.from_size
      | Named.Static_consts _consts -> Cost_metrics.zero
      | Named.Set_of_closures set_of_closures ->
        let code_mapping = Acc.code acc in
        Cost_metrics.set_of_closures 
          ~find_cost_metrics:(fun code_id ->
            Code_id.Map.find code_id code_mapping
            |> Code.cost_metrics)
          set_of_closures
    in
    let acc =
      Acc.increment_metrics 
        (Cost_metrics.increase_due_to_let_expr
           ~is_phantom:false
           ~cost_metrics_of_defining_expr)
        acc
    in
    acc, Let.create let_bound named ~body ~free_names_of_body
end

module Continuation_handler_wrapper = struct
  let create acc parameters ~handler ~free_names_of_handler ~is_exn_handler =
    acc,
    Continuation_handler.create parameters ~handler
      ~free_names_of_handler ~is_exn_handler
end

module Let_cont_wrapper = struct
  let create_non_recursive acc cont handler
        ~body ~free_names_of_body ~cost_metrics_of_handler =
    let acc =
      Acc.increment_metrics
        (Cost_metrics.increase_due_to_let_cont_non_recursive
           ~cost_metrics_of_handler)
        acc
    in
    acc,
    Let_cont.create_non_recursive cont handler  ~body ~free_names_of_body

  let create_recursive acc handlers ~body ~cost_metrics_of_handlers =
    let acc =
      Acc.increment_metrics
        (Cost_metrics.increase_due_to_let_cont_recursive
           ~cost_metrics_of_handlers)
        acc
    in
    acc, Let_cont.create_recursive handlers ~body
end

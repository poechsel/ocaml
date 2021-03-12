include Flambda.Import

module With_size = struct
  type 'a t = 'a * Cost_metrics.t

  let create ~size x = x, size

  let size (_, s) = s

  let get (d, _) = d
end

module Expr_with_size = struct
  type t = Expr.t With_size.t

  let create_apply_cont apply_cont =
    Expr.create_apply_cont apply_cont,
    Code_size.apply_cont apply_cont |> Cost_metrics.from_size

  let create_apply apply =
    Expr.create_apply apply,
    Code_size.apply apply |> Cost_metrics.from_size

  let create_let (let_expr, size) =
    Expr.create_let let_expr,
    size

  let create_switch switch =
    Expr.create_switch switch,
    Code_size.switch switch |> Cost_metrics.from_size

  let create_invalid ?semantics () =
    Expr.create_invalid ?semantics (),
    Code_size.invalid |> Cost_metrics.from_size
end

module Named_with_size = struct
  type t = Named.t With_size.t
  let create_simple simple =
    Named.create_simple simple,
    Code_size.simple simple |> Cost_metrics.from_size

  let create_prim prim dbg =
    Named.create_prim prim dbg,
    Code_size.prim prim |> Cost_metrics.from_size

  let create_set_of_closures ~find_cost_metrics set_of_closures =
    Named.create_set_of_closures set_of_closures,
    Cost_metrics.set_of_closures ~find_cost_metrics set_of_closures

  let create_static_consts static_consts =
    Named.create_static_consts static_consts,
    Code_size.zero |> Cost_metrics.from_size
end

module Let_with_size = struct
  let create let_bound named ~body ~free_names_of_body =
    Let.create let_bound (With_size.get named) ~body:(With_size.get body)
      ~free_names_of_body,
    Cost_metrics.(+)
      (* No phantom let are created inside closure conversion *)
      (Cost_metrics.increase_due_to_let_expr
         ~is_phantom:false
         ~cost_metrics_of_defining_expr:(With_size.size named))
      (With_size.size body)
end

module Continuation_handler_with_size = struct
  let create parameters ~handler ~free_names_of_handler ~is_exn_handler =
    Continuation_handler.create parameters ~handler:(With_size.get handler)
      ~free_names_of_handler ~is_exn_handler,
    With_size.size handler
end

module Let_cont_with_size = struct
  let create_non_recursive cont handler ~body ~free_names_of_body =
    Let_cont.create_non_recursive cont (With_size.get handler)
      ~body:(With_size.get body) ~free_names_of_body,
    Cost_metrics.(+)
      (Cost_metrics.increase_due_to_let_cont_non_recursive
         ~cost_metrics_of_handler:(With_size.size handler))
      (With_size.size body)

  let create_recursive handlers ~body =
    let handlers_size =
      Continuation.Map.fold
        (fun _ e acc -> Cost_metrics.(+) acc (With_size.size e))
        handlers (Cost_metrics.zero)
    in
    let handlers = Continuation.Map.map With_size.get handlers in
    Let_cont.create_recursive handlers ~body:(With_size.get body),
    Cost_metrics.(+)
      (Cost_metrics.increase_due_to_let_cont_recursive
         ~cost_metrics_of_handlers:handlers_size)
      (With_size.size body)
end

include Flambda.Import

module With_size = struct
  type 'a t = 'a * Cost_metrics.t

  let create ~size x = x, size

  let size (_, s) = s

  let get (d, _) = d
end

module Expr_with_size = struct
  type t = Expr.t With_size.t

  let create_apply_cont acc apply_cont =
    acc,
    (Expr.create_apply_cont apply_cont,
     Code_size.apply_cont apply_cont |> Cost_metrics.from_size)

  let create_apply acc apply =
    acc,
    (Expr.create_apply apply,
     Code_size.apply apply |> Cost_metrics.from_size)

  let create_let (acc, (let_expr, size)) =
    acc,
    (Expr.create_let let_expr,
     size)

  let create_switch acc switch =
    acc,
    (Expr.create_switch switch,
     Code_size.switch switch |> Cost_metrics.from_size)

  let create_invalid acc ?semantics () =
    acc,
    (Expr.create_invalid ?semantics (),
     Code_size.invalid |> Cost_metrics.from_size)
end

module Named_with_size = struct
  type t = Named.t With_size.t
  let create_simple acc simple =
    acc,
    (Named.create_simple simple,
     Code_size.simple simple |> Cost_metrics.from_size)

  let create_prim acc prim dbg =
    acc,
    (Named.create_prim prim dbg,
     Code_size.prim prim |> Cost_metrics.from_size)

  let create_set_of_closures acc ~find_cost_metrics set_of_closures =
    acc,
    (Named.create_set_of_closures set_of_closures,
     Cost_metrics.set_of_closures ~find_cost_metrics set_of_closures)

  let create_static_consts acc static_consts =
    acc,
    (Named.create_static_consts static_consts,
     Code_size.zero |> Cost_metrics.from_size)
end

module Let_with_size = struct
  let create acc let_bound named ~body ~free_names_of_body =
    acc,
    (Let.create let_bound (With_size.get named) ~body:(With_size.get body)
       ~free_names_of_body,
     Cost_metrics.(+)
       (* No phantom let are created inside closure conversion *)
       (Cost_metrics.increase_due_to_let_expr
          ~is_phantom:false
          ~cost_metrics_of_defining_expr:(With_size.size named))
       (With_size.size body))
end

module Continuation_handler_with_size = struct
  let create acc parameters ~handler ~free_names_of_handler ~is_exn_handler =
    acc,
    (Continuation_handler.create parameters ~handler:(With_size.get handler)
       ~free_names_of_handler ~is_exn_handler,
     With_size.size handler)
end

module Let_cont_with_size = struct
  let create_non_recursive acc cont handler ~body ~free_names_of_body =
    acc,
    (Let_cont.create_non_recursive cont (With_size.get handler)
       ~body:(With_size.get body) ~free_names_of_body,
     Cost_metrics.(+)
       (Cost_metrics.increase_due_to_let_cont_non_recursive
          ~cost_metrics_of_handler:(With_size.size handler))
       (With_size.size body))

  let create_recursive acc handlers ~body =
    let handlers_size =
      Continuation.Map.fold
        (fun _ e acc -> Cost_metrics.(+) acc (With_size.size e))
        handlers (Cost_metrics.zero)
    in
    let handlers = Continuation.Map.map With_size.get handlers in
    acc,
    (Let_cont.create_recursive handlers ~body:(With_size.get body),
     Cost_metrics.(+)
       (Cost_metrics.increase_due_to_let_cont_recursive
          ~cost_metrics_of_handlers:handlers_size)
       (With_size.size body))
end

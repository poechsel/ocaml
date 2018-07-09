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

module InliningArgs = Flambda.InliningArgs
module UnboxingArgs = Flambda.UnboxingArgs

module Env = struct
  type scope = Current | Outer

  type t = {
    backend : (module Backend_intf.S);
    round : int;
    approx : (scope * Simple_value_approx.t) Variable.Map.t;
    approx_mutable : Simple_value_approx.t Mutable_variable.Map.t;
    approx_sym : Simple_value_approx.t Symbol.Map.t;
    projections : Variable.t Projection.Map.t;
    (* The functions currently being declared: used to avoid inlining
       recursively *)
    speculation_depth : int;
    (* Number of times "inline" has been called recursively *)
    inside_branch : int;
    freshening : Freshening.t;
    never_inline : bool ;
    never_inline_inside_closures : bool;
    never_inline_outside_closures : bool;
    inlining_depth : int;
    specialise_depth : int;
    closure_depth : int;
    inlined_debuginfo : Debuginfo.t;
    inlining_arguments : InliningArgs.t;
    max_inlining_arguments : InliningArgs.t;
    inlining_history : Inlining_history.History.t;
    (* the current inlining history from the toplevel *)
    inlining_history_next_parts : Inlining_history.History.t;
    (* Next part of inlining history. Composed of the relative history
       we want to move down and store inside the next closure definition
       or apply node. Used after inlining a call for exemple. *)
  }
  let inlining_history_next_parts t = t.inlining_history_next_parts

  let create ~never_inline ~backend ~round =
    { backend;
      round;
      approx = Variable.Map.empty;
      approx_mutable = Mutable_variable.Map.empty;
      approx_sym = Symbol.Map.empty;
      projections = Projection.Map.empty;
      speculation_depth = 0;
      inside_branch = 0;
      freshening = Freshening.empty;
      never_inline;
      never_inline_inside_closures = false;
      never_inline_outside_closures = false;
      inlining_depth = 0;
      specialise_depth = 0;
      closure_depth = 0;
      inlined_debuginfo = Debuginfo.none;
      inlining_arguments = InliningArgs.get round;
      max_inlining_arguments = InliningArgs.get_max ();
      inlining_history = Inlining_history.History.empty;
      inlining_history_next_parts = Inlining_history.History.empty;
    }

  let backend t = t.backend
  let round t = t.round

  let local env =
    { env with
      approx = Variable.Map.empty;
      projections = Projection.Map.empty;
      freshening = Freshening.empty_preserving_activation_state env.freshening;
      inlined_debuginfo = Debuginfo.none;
    }

  let get_inlining_arguments env = env.inlining_arguments

  let get_max_inlining_arguments env = env.max_inlining_arguments

  let set_inlining_arguments env args = { env with inlining_arguments = args }

  let set_max_inlining_arguments env args =
    { env with max_inlining_arguments = args }

  let speculation_depth_up env =
    let args = InliningArgs.extract (get_inlining_arguments env) in
    let max_level = args.inline_max_speculation_depth in
    if (env.speculation_depth + 1) > max_level then
      Misc.fatal_error "Inlining level increased above maximum";
    { env with speculation_depth = env.speculation_depth + 1 }

  let print ppf t =
    Format.fprintf ppf
      "Environment maps: %a@.Projections: %a@.Freshening: %a@."
      Variable.Set.print (Variable.Map.keys t.approx)
      (Projection.Map.print Variable.print) t.projections
      Freshening.print t.freshening

  let mem t var = Variable.Map.mem var t.approx

  let add_internal t var (approx : Simple_value_approx.t) ~scope =
    let approx =
      (* The semantics of this [match] are what preserve the property
         described at the top of simple_value_approx.mli, namely that when a
         [var] is mem on an approximation (amongst many possible [var]s),
         it is the one with the outermost scope. *)
      match approx.var with
      | Some var when mem t var -> approx
      | _ -> Simple_value_approx.augment_with_variable approx var
    in
    { t with approx = Variable.Map.add var (scope, approx) t.approx }

  let add t var approx = add_internal t var approx ~scope:Current
  let add_outer_scope t var approx = add_internal t var approx ~scope:Outer

  let add_mutable t mut_var approx =
    { t with approx_mutable =
        Mutable_variable.Map.add mut_var approx t.approx_mutable;
    }

  let really_import_approx t =
    let module Backend = (val (t.backend) : Backend_intf.S) in
    Backend.really_import_approx

  let really_import_approx_with_scope t (scope, approx) =
    scope, really_import_approx t approx

  let find_symbol_exn t symbol =
    really_import_approx t
      (Symbol.Map.find symbol t.approx_sym)

  let find_symbol_opt t symbol =
    try Some (really_import_approx t
                (Symbol.Map.find symbol t.approx_sym))
    with Not_found -> None

  let find_symbol_fatal t symbol =
    match find_symbol_exn t symbol with
    | exception Not_found ->
      Misc.fatal_errorf "Symbol %a is unbound.  Maybe there is a missing \
          [Let_symbol], [Import_symbol] or similar?"
        Symbol.print symbol
    | approx -> approx

  let find_or_load_symbol t symbol =
    match find_symbol_exn t symbol with
    | exception Not_found ->
      if Compilation_unit.equal
          (Compilation_unit.get_current_exn ())
          (Symbol.compilation_unit symbol)
      then
        Misc.fatal_errorf "Symbol %a from the current compilation unit is \
            unbound.  Maybe there is a missing [Let_symbol] or similar?"
          Symbol.print symbol;
      let module Backend = (val (t.backend) : Backend_intf.S) in
      Backend.import_symbol symbol
    | approx -> approx

  let find_closure_id_for_symbol t symbol =
    let approx = find_or_load_symbol t symbol in
    match Simple_value_approx.check_approx_for_closure approx with
    | Ok (value_closure, _, _, _) -> Some value_closure.closure_id
    | Wrong -> None

  let add_projection t ~projection ~bound_to =
    { t with
      projections =
        Projection.Map.add projection bound_to t.projections;
    }

  let find_projection t ~projection =
    match Projection.Map.find projection t.projections with
    | exception Not_found -> None
    | var -> Some var

  let does_not_bind t vars =
    not (List.exists (mem t) vars)

  let does_not_freshen t vars =
    Freshening.does_not_freshen t.freshening vars

  let add_symbol t symbol approx =
    match find_symbol_exn t symbol with
    | exception Not_found ->
      { t with
        approx_sym = Symbol.Map.add symbol approx t.approx_sym;
      }
    | _ ->
      Misc.fatal_errorf "Attempt to redefine symbol %a (to %a) in environment \
          for [Inline_and_simplify]"
        Symbol.print symbol
        Simple_value_approx.print approx

  let redefine_symbol t symbol approx =
    match find_symbol_exn t symbol with
    | exception Not_found ->
      assert false
    | _ ->
      { t with
        approx_sym = Symbol.Map.add symbol approx t.approx_sym;
      }

  let find_with_scope_exn t id =
    try
      really_import_approx_with_scope t
        (Variable.Map.find id t.approx)
    with Not_found ->
      Misc.fatal_errorf "Env.find_with_scope_exn: Unbound variable \
          %a@.%s@. Environment: %a@."
        Variable.print id
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))
        print t

  let find_exn t id =
    snd (find_with_scope_exn t id)

  let find_mutable_exn t mut_var =
    try Mutable_variable.Map.find mut_var t.approx_mutable
    with Not_found ->
      Misc.fatal_errorf "Env.find_mutable_exn: Unbound variable \
          %a@.%s@. Environment: %a@."
        Mutable_variable.print mut_var
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))
        print t

  let find_list_exn t vars =
    List.map (fun var -> find_exn t var) vars

  let find_opt t id =
    try Some (really_import_approx t
                (snd (Variable.Map.find id t.approx)))
    with Not_found -> None

  let activate_freshening t =
    { t with freshening = Freshening.activate t.freshening }

  let at_toplevel t =
    t.closure_depth = 0

  let is_inside_branch env = env.inside_branch > 0

  let branch_depth env = env.inside_branch

  let inside_branch t =
    { t with inside_branch = t.inside_branch + 1 }

  let set_freshening t freshening  =
    { t with freshening; }

  let increase_closure_depth t =
    let approx =
      Variable.Map.map (fun (_scope, approx) -> Outer, approx) t.approx
    in
    { t with
      approx;
      closure_depth = t.closure_depth + 1;
    }

  let set_never_inline t =
    if t.never_inline then t
    else { t with never_inline = true }

  let set_never_inline_inside_closures t =
    if t.never_inline_inside_closures then t
    else { t with never_inline_inside_closures = true }

  let unset_never_inline_inside_closures t =
    if t.never_inline_inside_closures then
      { t with never_inline_inside_closures = false }
    else t

  let set_never_inline_outside_closures t =
    if t.never_inline_outside_closures then t
    else { t with never_inline_outside_closures = true }

  let unset_never_inline_outside_closures t =
    if t.never_inline_outside_closures then
      { t with never_inline_outside_closures = false }
    else t

  let inlining_allowed t =
    let limit =(InliningArgs.extract t.inlining_arguments).inline_max_depth
    in
    t.inlining_depth < limit

  let specialising_allowed t =
    let limit =
      (InliningArgs.extract t.inlining_arguments).inline_max_specialise
    in
    limit - t.specialise_depth > 0

  let inside_specialised_function t =
    { t with specialise_depth = t.specialise_depth + 1 }


  let inside_inlined_function t =
    let inlining_depth = t.inlining_depth + 1 in
    { t with inlining_depth }

  let clear_inlining_depth t =
    { t with inlining_depth = 0 }

  let add_original_inlining_depth t orig =
    let inlining_depth = orig + t.inlining_depth in
    { t with inlining_depth }

  let speculation_depth t = t.speculation_depth
  let inlining_depth t = t.inlining_depth
  let freshening t = t.freshening
  let never_inline t = t.never_inline || t.never_inline_outside_closures

  let pop_inlining_history_next_parts t =
    t.inlining_history_next_parts,
    { t with inlining_history_next_parts = Inlining_history.History.empty}

  let inlining_history t =
    t.inlining_history

  let set_inlining_history t history =
    { t with inlining_history = history }

  let add_inlining_history t substats =
    { t with inlining_history =
               Inlining_history.add
                 substats t.inlining_history
                 }

  let add_inlining_history_parts t parts =
    { t with inlining_history_next_parts =
               Inlining_history.add
                 parts t.inlining_history_next_parts }

  let add_inlining_history_part t part =
    { t with inlining_history_next_parts =
               part :: t.inlining_history_next_parts }

  let enter_closure t ~inline_inside =
    let t =
      if inline_inside && not t.never_inline_inside_closures then t
      else set_never_inline t
    in
    unset_never_inline_outside_closures t

  let record_decision t decision =
    Inlining_stats.record_decision decision
      ~closure_stack:t.inlining_history

  let record_definition t =
    if t.inlining_history <> Inlining_history.History.empty then
    Inlining_stats.record_decision Inlining_stats_types.Decision.Definition
      ~closure_stack:t.inlining_history

  let set_inline_debuginfo t ~dbg =
    { t with inlined_debuginfo = dbg }

  let add_inlined_debuginfo t ~dbg =
    Debuginfo.concat t.inlined_debuginfo dbg
end

let initial_inlining_threshold (inlining_arguments : InliningArgs.t)
  : Inlining_cost.Threshold.t =
  let unscaled = (InliningArgs.extract inlining_arguments).inline_threshold
  in
  (* CR-soon pchambart: Add a warning if this is too big
     mshinwell: later *)
  Can_inline_if_no_larger_than
    (int_of_float
      (unscaled *. float_of_int Inlining_cost.scale_inline_threshold_by))

let initial_inlining_toplevel_threshold (inlining_arguments : InliningArgs.t)
  : Inlining_cost.Threshold.t =
  let ordinary_threshold =
    (InliningArgs.extract inlining_arguments).inline_threshold
  in
  let toplevel_threshold =
    (InliningArgs.extract inlining_arguments).inline_toplevel_threshold
  in
  let unscaled =
    (int_of_float ordinary_threshold) + toplevel_threshold
  in
  (* CR-soon pchambart: Add a warning if this is too big
     mshinwell: later *)
  Can_inline_if_no_larger_than
    (unscaled * Inlining_cost.scale_inline_threshold_by)

module Result = struct
  type t =
    { approx : Simple_value_approx.t;
      used_static_exceptions : Static_exception.Set.t;
      inlining_threshold : Inlining_cost.Threshold.t option;
      benefit : Inlining_cost.Benefit.t;
      num_direct_applications : int;
    }

  let create () =
    { approx = Simple_value_approx.value_unknown Other;
      used_static_exceptions = Static_exception.Set.empty;
      inlining_threshold = None;
      benefit = Inlining_cost.Benefit.zero;
      num_direct_applications = 0;
    }

  let approx t = t.approx
  let set_approx t approx = { t with approx }

  let meet_approx t env approx =
    let really_import_approx = Env.really_import_approx env in
    let meet =
      Simple_value_approx.meet ~really_import_approx t.approx approx
    in
    set_approx t meet

  let use_static_exception t i =
    { t with
      used_static_exceptions =
        Static_exception.Set.add i t.used_static_exceptions;
    }

  let used_static_exceptions t = t.used_static_exceptions

  let exit_scope_catch t i =
    { t with
      used_static_exceptions =
        Static_exception.Set.remove i t.used_static_exceptions;
    }

  let map_benefit t f =
    { t with benefit = f t.benefit }

  let add_benefit t b =
    { t with benefit = Inlining_cost.Benefit.(+) t.benefit b }

  let benefit t = t.benefit

  let reset_benefit t =
    { t with benefit = Inlining_cost.Benefit.zero; }

  let set_inlining_threshold t inlining_threshold =
    { t with inlining_threshold }

  let add_inlining_threshold t j =
    match t.inlining_threshold with
    | None -> t
    | Some i ->
      let inlining_threshold = Some (Inlining_cost.Threshold.add i j) in
      { t with inlining_threshold }

  let sub_inlining_threshold t j =
    match t.inlining_threshold with
    | None -> t
    | Some i ->
      let inlining_threshold = Some (Inlining_cost.Threshold.sub i j) in
      { t with inlining_threshold }

  let inlining_threshold t = t.inlining_threshold

  let seen_direct_application t =
    { t with num_direct_applications = t.num_direct_applications + 1; }

  let num_direct_applications t =
    t.num_direct_applications
end

module A = Simple_value_approx
module E = Env

let keep_body_check ~is_classic_mode =
  if not (is_classic_mode > 0.0) then begin
      fun _ -> true
  end else begin
    let can_inline_non_rec_function (fun_decl : Flambda.function_declaration) =
      (* In classic-inlining mode, the inlining decision is taken at
         definition site (here). If the function is small enough
         (below the -inline threshold) it will always be inlined.

         Closure gives a bonus of [8] to optional arguments. In classic
         mode, however, we would inline functions with the "*opt*" argument
         in all cases, as it is a stub. (This is ensured by
         [middle_end/closure_conversion.ml]).
      *)

      let inlining_threshold =
        Inlining_cost.Threshold.Can_inline_if_no_larger_than
          (int_of_float
             (is_classic_mode *. float_of_int Inlining_cost.scale_inline_threshold_by))
      in
      let bonus = Flambda_utils.function_arity fun_decl in
      Inlining_cost.can_inline fun_decl.body inlining_threshold ~bonus
    in
    fun (fun_decl : Flambda.function_declaration) ->
      if fun_decl.stub then begin
        true
      end else if fun_decl.recursive then begin
        false
      end else begin
        match fun_decl.inline with
        | Default_inline -> can_inline_non_rec_function fun_decl
        | Unroll factor -> factor > 0
        | Always_inline -> true
        | Never_inline -> false
      end
    end

let prepare_to_simplify_set_of_closures ~env
      ~(set_of_closures : Flambda.set_of_closures)
      ~function_decls ~freshen
      ~(only_for_function_decl : Flambda.function_declaration option) =
  assert(E.inlining_history_next_parts env = Inlining_history.History.empty);
  let free_vars =
    Variable.Map.map (fun (external_var : Flambda.specialised_to) ->
        let var =
          let var =
            Freshening.apply_variable (E.freshening env) external_var.var
          in
          match
            A.simplify_var_to_var_using_env (E.find_exn env var)
              ~is_present_in_env:(fun var -> E.mem env var)
          with
          | None -> var
          | Some var -> var
        in
        let approx = E.find_exn env var in
        (* The projections are freshened below in one step, once we know
           the closure freshening substitution. *)
        let projection = external_var.projection in
        ({ var; projection; } : Flambda.specialised_to), approx)
      set_of_closures.free_vars
  in
  let specialised_args =
    Variable.Map.filter_map set_of_closures.specialised_args
      ~f:(fun param (spec_to : Flambda.specialised_to) ->
        let keep =
          match only_for_function_decl with
          | None -> true
          | Some function_decl ->
            Variable.Set.mem param (Parameter.Set.vars function_decl.params)
        in
        if not keep then None
        else
          let external_var = spec_to.var in
          let var =
            Freshening.apply_variable (E.freshening env) external_var
          in
          let var =
            match
              A.simplify_var_to_var_using_env (E.find_exn env var)
                ~is_present_in_env:(fun var -> E.mem env var)
            with
            | None -> var
            | Some var -> var
          in
          let projection = spec_to.projection in
          Some ({ var; projection; } : Flambda.specialised_to))
  in
  let environment_before_cleaning = env in
  (* [E.local] helps us to catch bugs whereby variables escape their scope. *)
  let env = E.local env in
  let free_vars, function_decls, sb, freshening =
    Freshening.apply_function_decls_and_free_vars (E.freshening env) free_vars
      function_decls ~only_freshen_parameters:(not freshen)
  in
  let env = E.set_freshening env sb in
  let free_vars =
    Freshening.freshen_projection_relation' free_vars
      ~freshening:(E.freshening env)
      ~closure_freshening:freshening
  in
  let specialised_args =
    let specialised_args =
      Variable.Map.map_keys (Freshening.apply_variable (E.freshening env))
        specialised_args
    in
    Freshening.freshen_projection_relation specialised_args
      ~freshening:(E.freshening env)
      ~closure_freshening:freshening
  in
  let parameter_approximations =
    (* Approximations of parameters that are known to always hold the same
       argument throughout the body of the function. *)
    Variable.Map.map_keys (Freshening.apply_variable (E.freshening env))
      (Variable.Map.mapi (fun _id' (spec_to : Flambda.specialised_to) ->
          E.find_exn environment_before_cleaning spec_to.var)
        specialised_args)
  in
  let direct_call_surrogates =
    Variable.Map.fold (fun existing surrogate surrogates ->
        let existing =
          Freshening.Project_var.apply_closure_id freshening
            (Closure_id.wrap existing)
        in
        let surrogate =
          Freshening.Project_var.apply_closure_id freshening
            (Closure_id.wrap surrogate)
        in
        assert (not (Closure_id.Map.mem existing surrogates));
        Closure_id.Map.add existing surrogate surrogates)
      set_of_closures.direct_call_surrogates
      Closure_id.Map.empty
  in
  (* we use the previous closure for evaluating the functions *)
  let internal_value_set_of_closures =
    let bound_vars =
      Variable.Map.fold (fun id (_, desc) map ->
          Var_within_closure.Map.add (Var_within_closure.wrap id) desc map)
        free_vars Var_within_closure.Map.empty
    in
    let rec_info = set_of_closures.rec_info in
    let free_vars = Variable.Map.map fst free_vars in
    let invariant_params = lazy Variable.Map.empty in
    let is_classic_mode = function_decls.is_classic_mode in
    let keep_body = keep_body_check ~is_classic_mode in
    let function_decls =
      A.function_declarations_approx ~keep_body function_decls
        ~full_history:(E.inlining_history env)
    in
    A.create_value_set_of_closures ~function_decls ~bound_vars
      ~rec_info ~free_vars ~invariant_params ~specialised_args
      ~freshening ~direct_call_surrogates
      ~args:(Env.get_inlining_arguments env)
      ~unboxing_arguments:set_of_closures.unboxing_arguments
  in
  (* Populate the environment with the approximation of each closure.
     If [recursive] is true, this part of the environment is shared between all
     the recursive closures in this set of closures; otherwise, it's shared
     between the non-recursive ones. *)
  let closure_env ~recursive =
    Variable.Map.fold (fun closure (decl : Flambda.function_declaration) env ->
        let rec_info : Flambda.rec_info = {
          unroll_to = 0;
          depth = if recursive && decl.recursive then 1 else 0;
        }
        in
        let approx =
          A.value_closure ~closure_var:closure ~rec_info
            internal_value_set_of_closures
            (Closure_id.wrap closure)
        in
        E.add env closure approx
      )
      function_decls.funs env
  in
  let nonrec_closure_env = lazy (closure_env ~recursive:false) in
  let rec_closure_env = lazy (closure_env ~recursive:true) in
  free_vars, specialised_args, function_decls, parameter_approximations,
    internal_value_set_of_closures, nonrec_closure_env, rec_closure_env

(* This adds only the minimal set of approximations to the closures.
   It is not strictly necessary to have this restriction, but it helps
   to catch potential substitution bugs. *)
let populate_closure_approximations
      ~(function_decl : Flambda.function_declaration)
      ~(free_vars : (_ * A.t) Variable.Map.t)
      ~(parameter_approximations : A.t Variable.Map.t)
      ~env =
  (* Add approximations of free variables *)
  let env =
    Variable.Map.fold (fun id (_, desc) env ->
        E.add_outer_scope env id desc)
      free_vars env
  in
  (* Add known approximations of function parameters *)
  let env =
    List.fold_left (fun env id ->
        let approx =
          try Variable.Map.find id parameter_approximations
          with Not_found -> (A.value_unknown Other)
        in
        E.add env id approx)
      env (Parameter.List.vars function_decl.params)
  in
  env

let prepare_to_simplify_closure ~(function_decl : Flambda.function_declaration)
      ~free_vars ~specialised_args ~parameter_approximations
      ~nonrec_closure_env ~rec_closure_env =
  let closure_env =
    if function_decl.recursive
    then Lazy.force rec_closure_env
    else Lazy.force nonrec_closure_env
  in
  let closure_env =
    populate_closure_approximations ~function_decl ~free_vars
      ~parameter_approximations ~env:closure_env
  in
  (* Add definitions of known projections to the environment. *)
  let add_projections ~closure_env ~which_variables ~map =
    Variable.Map.fold (fun inner_var spec_arg env ->
        let (spec_arg : Flambda.specialised_to) = map spec_arg in
        match spec_arg.projection with
        | None -> env
        | Some projection ->
          let from = Projection.projecting_from projection in
          if Variable.Set.mem from function_decl.free_variables then
            E.add_projection env ~projection ~bound_to:inner_var
          else
            env)
      which_variables
      closure_env
  in
  let closure_env =
    add_projections ~closure_env ~which_variables:specialised_args
      ~map:(fun spec_to -> spec_to)
  in
  add_projections ~closure_env ~which_variables:free_vars
    ~map:(fun (spec_to, _approx) -> spec_to)

let rewrite_recursive_calls_with_symbols env
      (function_declarations : Flambda.function_declarations) =
  let get_funs (decls : Flambda.function_declarations) = decls.funs in
  let get_free_symbols (decl : Flambda.function_declaration) =
    decl.free_symbols
  in
  let update_function_declaration_body =
    Flambda.update_function_declaration_body
  in
  let update_function_declarations =
    Flambda.update_function_declarations
  in
  let symbol_to_closure_id = E.find_closure_id_for_symbol env in
  Freshening.rewrite_recursive_calls_with_symbols (E.freshening env)
    function_declarations
    ~get_funs ~get_free_symbols
    ~update_function_declaration_body ~update_function_declarations
    ~symbol_to_closure_id

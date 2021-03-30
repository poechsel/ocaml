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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

module DE = Downwards_env
module DA = Downwards_acc

let is_it_a_small_function ~round cost_metrics =
  let small_function_size =
    Clflags.Int_arg_helper.get ~key:round !Clflags.inline_small_function_size
  in
  Cost_metrics.size cost_metrics
  |> Code_size.smaller_than ~size:small_function_size

let get_big_function_size ~round =
  Clflags.Int_arg_helper.get ~key:round !Clflags.inline_big_function_size

let is_it_a_big_function ~round cost_metrics =
  Cost_metrics.size cost_metrics
  |> Code_size.smaller_than ~size:(get_big_function_size ~round)

let is_it_under_inline_threshold ~round cost_metrics =
  let threshold =
    Clflags.Float_arg_helper.get ~key:round !Clflags.inline_threshold
  in
  Float.compare (Cost_metrics.evaluate ~round cost_metrics) threshold <= 0

(* CR mshinwell: We need to emit [Warnings.Inlining_impossible] as
   required.
   When in fallback-inlining mode: if we want to follow Closure we should
   not complain about function declarations with e.g. [@inline always]
   if the function contains other functions and therefore cannot be
   inlined.  We should however contain at call sites if inlining is
   requested but cannot be done for this reason.  I think this will probably
   all happen without any specific code once [Inlining_impossible]
   handling is implemented for the non-fallback-inlining cases. *)

module Function_declaration_decision = struct
  type t =
    | Never_inline_attribute
    | Function_body_too_large of Code_size.t
    | Stub
    | Inline of (Code_size.t * Code_size.t) option

  let can_inline t =
    match t with
    | Never_inline_attribute
    | Function_body_too_large _ -> false
    | Stub
    | Inline _ -> true

  let print fmt = function
    | Never_inline_attribute ->
      Format.fprintf fmt "Never_inline_attribute"
    | Function_body_too_large big_function_size ->
      Format.fprintf fmt "Function_body_too_large(%a)"
        Code_size.print big_function_size
    | Stub ->
      Format.fprintf fmt "Stub"
    | Inline None ->
      Format.fprintf fmt "Inline_no_cost_computed"
    | Inline Some (size, big_function_size) ->
      Format.fprintf fmt "Inline(size=%a, %a)"
        Code_size.print size
        Code_size.print big_function_size

  let report_reason fmt = function
    | Never_inline_attribute ->
      Format.fprintf fmt "%a"
        Format.pp_print_text "the function has an attribute preventing its inlining"
    | Function_body_too_large big_function_size ->
      Format.fprintf fmt "the@ function's@ body@ is@ too@ large,@ \
                          more@ specifically,@ it@ is@ larger@ than@ \
                          the@ big@ function@ size:@ %a"
        Code_size.print big_function_size
    | Stub ->
      Format.fprintf fmt "the@ function@ is@ a@ stub"
    | Inline None ->
      Format.fprintf fmt "the@ function@ has@ an@ attribute@ forcing@ its@ inlining"
    | Inline Some (size, big_function_size) ->
      Format.fprintf fmt "the@ function's@ body@ is@ smaller@ \
                          than@ the@ big_function_size:@ size=%a < \
                          big@ function@ size=%a"
        Code_size.print size Code_size.print big_function_size

  let report fmt t =
    Format.fprintf fmt "@[<v>The function %s be inlined at its use-sites@ \
                        because @[<hov>%a@]@]"
      (if can_inline t then "can" else "cannot") report_reason t

end

type cost_metrics_source = From_denv | Metrics of Cost_metrics.t

let make_decision_for_function_declaration denv ~cost_metrics_source function_decl
  : Function_declaration_decision.t =
  (* At present, we follow Closure, taking inlining decisions without
     first examining call sites. *)
  let code_id = Function_declaration.code_id function_decl in
  let code = DE.find_code denv code_id in
  match Code.inline code with
  | Never_inline -> Never_inline_attribute
  | Hint_inline | Always_inline -> Inline None
  | Default_inline | Unroll _ ->
    if Code.stub code then Stub
    else
      let round = DE.round denv in
      let metrics =
        match cost_metrics_source with
        | Metrics metrics -> metrics
        | From_denv -> Code.cost_metrics code
      in
      if is_it_a_big_function ~round metrics then
        Inline (Some (Cost_metrics.size metrics,
                      Code_size.of_int (get_big_function_size ~round)))
      else
        Function_body_too_large (Code_size.of_int (get_big_function_size ~round))

module Call_site_decision = struct
  type attribute_causing_inlining =
    | Unroll
    | Always

  let print_attribute_causing_inlining ppf attr =
    match attr with
    | Unroll -> Format.fprintf ppf "Unroll"
    | Always -> Format.fprintf ppf "Always"

  type t =
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

  let print ppf t =
    match t with
    | Environment_says_never_inline ->
      Format.fprintf ppf "Environment_says_never_inline"
    | Unrolling_depth_exceeded ->
      Format.fprintf ppf "Unrolling_depth_exceeded"
    | Max_inlining_depth_exceeded ->
      Format.fprintf ppf "Max_inlining_depth_exceeded"
    | Recursion_depth_exceeded ->
      Format.fprintf ppf "Recursion_depth_exceeded"
    | Never_inline_attribute ->
      Format.fprintf ppf "Never_inline_attribute"
    | Rejected_by_cost_metrics ->
      Format.fprintf ppf "Rejected_by_cost_metrics"
    | Inline { attribute; unroll_to; } ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(attribute@ %a)@]@ \
          @[<hov 1>(unroll_to@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print print_attribute_causing_inlining) attribute
        (Misc.Stdlib.Option.print Numbers.Int.print) unroll_to

  type can_inline =
    | Do_not_inline
    | Inline of { unroll_to : int option; }

  let can_inline (t : t) : can_inline =
    match t with
    | Environment_says_never_inline
    | Unrolling_depth_exceeded
    | Max_inlining_depth_exceeded
    | Recursion_depth_exceeded
    | Rejected_by_cost_metrics
    | Never_inline_attribute -> Do_not_inline
    | Inline { attribute = _; unroll_to; } -> Inline { unroll_to; }


  let report_reason fmt t =
    match (t : t) with
    | Environment_says_never_inline ->
      Format.fprintf fmt "the@ environment@ says@ never@ to@ inline"
    | Unrolling_depth_exceeded ->
      Format.fprintf fmt "the@ maximum@ unrolling@ depth@ has@ been@ exceeded"
    | Max_inlining_depth_exceeded ->
      Format.fprintf fmt "the@ maximum@ inlining@ depth@ has@ been@ exceeded"
    | Recursion_depth_exceeded ->
      Format.fprintf fmt "the@ maximum@ recursion@ depth@ has@ been@ exceeded"
    | Never_inline_attribute ->
      Format.fprintf fmt "the@ call@ has@ an@ attribute@ forbidding@ inlining"
    | Rejected_by_cost_metrics ->
      Format.fprintf fmt "the@ cost@ metrics@ were@ not@ high@ enough"
    | Inline { attribute = None; unroll_to = None; } ->
      Format.fprintf fmt "the@ function@ was@ deemed@ inlinable@ from@ its@ declaration"
    | Inline { attribute = Some Always; unroll_to = _; } ->
      Format.fprintf fmt "the@ call@ has@ an@ [@@inline always]@ attribute"
    | Inline { attribute = Some Unroll; unroll_to = Some n; } ->
      Format.fprintf fmt "the@ call@ has@ an@ [@@unroll %d]@ attribute" n

    (* this should not happen *)
    | Inline { attribute = None; unroll_to = Some _; }
    | Inline { attribute = Some Unroll; unroll_to = None; }
      ->
      Misc.fatal_errorf "This should not happen (Inlining_decision.report is not in sync\
                         with Inlining_decision.make_decision_for_call_site)"

  let report fmt t =
    Format.fprintf fmt "@[<v>The function call %s been inlined@ because @[<hov>%a@]@]"
      (match can_inline t with
       | Inline _ -> "has"
       | Do_not_inline -> "has not")
      report_reason t

end

(* CR mshinwell: Overhaul handling of the inlining depth tracking so that
   it takes into account the depth of closures (or code), as per
   conversation with lwhite. *)

(* CR mshinwell: This parameter needs to be configurable *)
let max_rec_depth = 1

module I = Flambda_type.Function_declaration_type.Inlinable

let make_decision_for_call_site dacc ~simplify_expr ~function_decl
      ~function_decl_rec_info ~apply : Call_site_decision.t =
  let get_inlined_cost_metrics ~unroll_to dacc =
    let _, expr =
      Inlining_transforms.inline dacc ~apply ~unroll_to function_decl
    in
    let dacc = DA.set_do_not_rebuild_terms dacc in
    let dacc =
      DA.denv dacc
      |> DE.disable_function_inlining
      |> DA.with_denv dacc
    in
    let _, uacc =
      simplify_expr dacc expr
        ~down_to_up:(fun dacc ~rebuild ->
          let uacc = Upwards_acc.create Upwards_env.empty dacc in
          rebuild uacc ~after_rebuild:(fun expr uacc -> expr, uacc)
        )
    in
    Upwards_acc.cost_metrics uacc
  in
  let force_inline ~attribute ~unroll_to : Call_site_decision.t =
      Inline { attribute; unroll_to }
  in
  let might_inline ~attribute ~unroll_to : Call_site_decision.t =
    let denv = DA.denv dacc in
    let code_id = I.code_id function_decl in
    let code = DE.find_code denv code_id in
    let cost_metrics = Code.cost_metrics code in
    let round = DE.round denv in
    let is_it_a_small_function = is_it_a_small_function ~round cost_metrics in
    let env_prohibits_inlining = not (DE.can_inline denv) in
    if is_it_a_small_function then
      force_inline ~attribute ~unroll_to
    else if env_prohibits_inlining then
      Environment_says_never_inline
    else
      let cost_metrics = get_inlined_cost_metrics ~unroll_to dacc in
      if is_it_under_inline_threshold ~round cost_metrics then
        force_inline ~attribute ~unroll_to
      else
        Rejected_by_cost_metrics

  in
  let inline = Apply.inline apply in
  match inline with
  | Never_inline -> Never_inline_attribute
  | Default_inline | Unroll _ | Always_inline | Hint_inline ->
    match Rec_info.unroll_to function_decl_rec_info with
    | Some unroll_to ->
      if Rec_info.depth function_decl_rec_info >= unroll_to then
        Unrolling_depth_exceeded
      else
        might_inline ~attribute:None ~unroll_to:None
    | None ->
      let apply_inlining_state = Apply.inlining_state apply in
      if Inlining_state.is_depth_exceeded apply_inlining_state
      then
        Max_inlining_depth_exceeded
      else
        match inline with
        | Never_inline -> assert false
        | Default_inline ->
          if Rec_info.depth function_decl_rec_info >= max_rec_depth then
            Recursion_depth_exceeded
          else
            might_inline ~attribute:None ~unroll_to:None
        | Unroll unroll_to ->
          let unroll_to =
            Rec_info.depth function_decl_rec_info + unroll_to
          in
          might_inline ~attribute:(Some Unroll) ~unroll_to:(Some unroll_to)
        | Always_inline | Hint_inline ->
          force_inline ~attribute:(Some Always) ~unroll_to:None

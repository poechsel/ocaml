(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

(* CR mshinwell: Need to simplify each [dbg] we come across. *)
(* CR mshinwell: Consider defunctionalising to remove the [k]. *)
(* CR mshinwell: May in any case be able to remove the polymorphic recursion. *)
(* CR mshinwell: See whether resolution of continuation aliases can be made
   more transparent (e.g. through [find_continuation]).  Tricky potentially in
   conjunction with the rewrites. *)

let rec simplify_expr dacc expr ~down_to_up =
  match Expr.descr expr with
  | Let let_expr -> simplify_let dacc let_expr ~down_to_up
  | Let_cont let_cont ->
    Simplify_let_cont_expr.simplify_let_cont ~simplify_expr dacc let_cont
      ~down_to_up
  | Apply apply ->
    Simplify_apply_expr.simplify_apply ~simplify_expr dacc apply ~down_to_up
  | Apply_cont apply_cont ->
    Simplify_apply_cont_expr.simplify_apply_cont dacc apply_cont ~down_to_up
  | Switch switch ->
    Simplify_switch_expr.simplify_switch ~simplify_let dacc switch ~down_to_up
  | Invalid _ ->
    (* CR mshinwell: Make sure that a program can be simplified to just
       [Invalid].  [Un_cps] should translate any [Invalid] that it sees as if
       it were [Halt_and_catch_fire]. *)
    down_to_up dacc ~rebuild:EB.rebuild_invalid

and simplify_toplevel dacc expr ~return_continuation
      ~return_arity exn_continuation ~return_cont_scope ~exn_cont_scope =
  let expr, uacc =
    simplify_expr dacc expr ~down_to_up:(fun dacc ~rebuild ->
      let uenv =
        UE.add_return_continuation UE.empty return_continuation
          return_cont_scope return_arity
      in
      let uenv =
        UE.add_exn_continuation uenv exn_continuation exn_cont_scope
      in
      let uacc = UA.create uenv dacc in
      rebuild uacc ~after_rebuild:(fun expr uacc -> expr, uacc))
  in
  (* We don't check occurrences of variables or symbols here because the check
     required depends on whether we're dealing with a lambda or the whole
     compilation unit.  Instead these checks are in [Simplify] or
     [Simplify_set_of_closures]. *)
  Name_occurrences.fold_continuations_including_in_trap_actions
    (UA.name_occurrences uacc)
    ~init:()
    ~f:(fun () cont ->
      let exn_continuation =
        Exn_continuation.exn_handler exn_continuation
      in
      if (not (Continuation.equal cont return_continuation))
        && (not (Continuation.equal cont exn_continuation))
      then begin
        Misc.fatal_errorf "Continuation %a should not be free in \
            toplevel expression after simplification (return \
            continuation %a, exn continuation %a):@ %a"
          Continuation.print cont
          (RE.print (UA.are_rebuilding_terms uacc)) expr
          Continuation.print return_continuation
          Continuation.print exn_continuation
      end);
  expr, uacc

and [@inline always] simplify_let dacc let_expr ~down_to_up =
  Simplify_let_expr.simplify_let ~simplify_expr
    ~simplify_toplevel dacc let_expr ~down_to_up

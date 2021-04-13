(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*   Copyright 2021--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

type t =
  | Unchanged
  | Poly_compare_specialized of DA.t * Expr.t

(* Helpers *)
(* ******* *)

let fun_symbol simple =
  let fail simple =
    Misc.fatal_errorf
      "Expected a function symbol, instead of@ %a" Simple.print simple
  in
  Simple.pattern_match simple
    ~name:(fun name ->
      Name.pattern_match name
        ~var:(fun _ -> fail simple)
        ~symbol:(fun sym -> sym))
    ~const:(fun _ -> fail simple)

let apply_cont cont v ~dbg =
  let args = [ Simple.name (Name.var v) ] in
  let apply_cont = Apply_cont.create cont ~args ~dbg in
  let free_names = Apply_cont.free_names apply_cont in
  let expr = Expr.create_apply_cont apply_cont in
  free_names, expr

let let_prim ~dbg v prim (free_names, body) =
  let v' = Var_in_binding_pos.create v Name_mode.normal in
  let bindable = Bindable_let_bound.singleton v' in
  let named = Named.create_prim prim dbg in
  let free_names_of_body = Or_unknown.Known free_names in
  let let_expr = Let.create bindable named ~body ~free_names_of_body in
  let free_names = Name_occurrences.remove_var free_names v in
  let expr = Expr.create_let let_expr in
  free_names, expr

let is_proved proof =
  match (proof : unit T.proof_allowing_kind_mismatch) with
  | Proved () -> true
  | Unknown | Invalid | Wrong_kind -> false


(* Exported simplification function *)
(* ******************************** *)

let simplify_comparison ~dbg ~dacc ~cont
      ~int_prim ~float_prim a b a_ty b_ty =
  let tenv = DA.typing_env dacc in
  if is_proved (T.prove_is_a_tagged_immediate tenv a_ty) &&
     is_proved (T.prove_is_a_tagged_immediate tenv b_ty) then begin
    let v_comp = Variable.create "comp" in
    let tagged = Variable.create "tagged" in
    let _, res =
      let_prim ~dbg v_comp (P.Binary (int_prim, a, b)) @@
      let_prim ~dbg tagged
        (P.Unary (Box_number Untagged_immediate, Simple.var v_comp)) @@
      apply_cont ~dbg cont tagged
    in
    Poly_compare_specialized (dacc, res)
  end else if is_proved (T.prove_is_a_boxed_float tenv a_ty) &&
              is_proved (T.prove_is_a_boxed_float tenv b_ty) then begin
    let a_naked = Variable.create "naked_float" in
    let b_naked = Variable.create "naked_float" in
    let v_comp = Variable.create "comp" in
    let tagged = Variable.create "tagged" in
    let _, res =
      let_prim ~dbg a_naked (P.Unary (Unbox_number Naked_float, a)) @@
      let_prim ~dbg b_naked (P.Unary (Unbox_number Naked_float, b)) @@
      let_prim ~dbg v_comp
        (P.Binary (float_prim, Simple.var a_naked, Simple.var b_naked)) @@
      let_prim ~dbg tagged
        (P.Unary (Box_number Untagged_immediate, Simple.var v_comp)) @@
      apply_cont ~dbg cont tagged
    in
    Poly_compare_specialized (dacc, res)
  end else
    Unchanged


let simplify_returning_extcall
      ~dbg ~cont ~exn_cont:_ dacc fun_name args ~arg_types =
  match fun_name, args, arg_types with
  (* Polymorphic comparisons *)
  | ".extern__caml_compare", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp Yielding_int_like_compare_functions)
      ~int_prim:(Int_comp (Tagged_immediate, Signed, Yielding_int_like_compare_functions))
  | ".extern__caml_equal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~int_prim:(Phys_equal (K.value, Eq))
      ~float_prim:(Float_comp (Yielding_bool Eq))
  | ".extern__caml_notequal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~int_prim:(Phys_equal (K.value, Neq))
      ~float_prim:(Float_comp (Yielding_bool Neq))
  | ".extern__caml_lessequal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool Le))
      ~int_prim:(Int_comp (Tagged_immediate, Signed, Yielding_bool Le))
  | ".extern__caml_lessthan", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool Lt))
      ~int_prim:(Int_comp (Tagged_immediate, Signed, Yielding_bool Lt))
  | ".extern__caml_greaterequal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool Ge))
      ~int_prim:(Int_comp (Tagged_immediate, Signed, Yielding_bool Ge))
  | ".extern__caml_greaterthan", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool Gt))
      ~int_prim:(Int_comp (Tagged_immediate, Signed, Yielding_bool Gt))

  (* Catchall *)
  | _ -> Unchanged


(* Exported simplification function *)
(* ******************************** *)

let simplify_extcall dacc apply ~callee_ty:_
      ~param_arity:_ ~return_arity:_ ~arg_types =
  let dbg = Apply.dbg apply in
  let args = Apply.args apply in
  let exn_cont = Apply.exn_continuation apply in
  let fun_name =
    Apply.callee apply
    |> fun_symbol
    |> Symbol.linkage_name
    |> Linkage_name.to_string
  in
  match Apply.continuation apply with
  | Never_returns -> Unchanged
  | Return cont ->
    simplify_returning_extcall ~dbg ~cont ~exn_cont dacc fun_name args ~arg_types



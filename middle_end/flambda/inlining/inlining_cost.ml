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

open! Flambda.Import

(* Simple approximation of the space cost of an Flambda expression. *)

let smaller' denv expr ~than:threshold =
  let s =
    Cost_metrics.expr_size
      ~find_code:(Downwards_env.find_code denv)
      expr
  in
  if Cost_metrics.smaller_than_threshold s ~threshold then
    Some (Cost_metrics.size s)
  else None

let size denv expr =
  match smaller' denv expr ~than:max_int with
  | Some size -> size
  | None ->
    (* There is no way that an expression of size max_int could fit in
       memory. *)
    assert false  (* CR mshinwell: this should not be an assertion *)

(*
let sizes exprs =
  List.fold_left (fun total expr -> total + size expr) 0 exprs
*)

module Threshold = struct
  type t =
    | Never_inline
    | Can_inline_if_no_larger_than of int

  let print fmt = function
    | Never_inline ->
      Format.fprintf fmt "Never_inline"
    | Can_inline_if_no_larger_than max_size ->
      Format.fprintf fmt "Can_inline_if_no_larger_than %d" max_size

  let add t1 t2 =
    match t1, t2 with
    | Never_inline, t -> t
    | t, Never_inline -> t
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
        Can_inline_if_no_larger_than (i1 + i2)

  let sub t1 t2 =
    match t1, t2 with
    | Never_inline, _ -> Never_inline
    | t, Never_inline -> t
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
        if i1 > i2 then Can_inline_if_no_larger_than (i1 - i2)
        else Never_inline

  let min t1 t2 =
    match t1, t2 with
    | Never_inline, _ -> Never_inline
    | _, Never_inline -> Never_inline
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
      Can_inline_if_no_larger_than (min i1 i2)
end

type inline_res =
  | Cannot_inline
  | Can_inline of int
  (* size of the inlinable expression, used in inlining reports *)

let can_inline denv lam inlining_threshold ~bonus : inline_res =
  match inlining_threshold with
  | Threshold.Never_inline -> Cannot_inline
  | Threshold.Can_inline_if_no_larger_than inlining_threshold ->
    begin match smaller' denv lam
                  ~than:(inlining_threshold + bonus) with
    | None -> Cannot_inline
    | Some size -> Can_inline size
    end

let cost (flag : Clflags.Int_arg_helper.parsed) ~round =
  Clflags.Int_arg_helper.get ~key:round flag

let benefit_factor = 1

module Benefit = struct
  type t = {
    remove_call : int;
    remove_alloc : int;
    remove_prim : int;
    remove_branch : int;
    (* CR-someday pchambart: branch_benefit : t list; *)
    direct_call_of_indirect : int;
    requested_inline : int;
    (* Benefit to compensate the size of functions marked for inlining *)
  }

  let zero = {
    remove_call = 0;
    remove_alloc = 0;
    remove_prim = 0;
    remove_branch = 0;
    direct_call_of_indirect = 0;
    requested_inline = 0;
  }

  let remove_call t = { t with remove_call = t.remove_call + 1; }
  let remove_alloc t = { t with remove_alloc = t.remove_alloc + 1; }

  let add_primitive _prim t =
    { t with remove_prim = t.remove_prim - 1; }

  let remove_primitive _prim t =
    { t with remove_prim = t.remove_prim + 1; }

  let remove_primitive_application _prim t =
    { t with remove_prim = t.remove_prim + 1; }

  let remove_branch t = { t with remove_branch = t.remove_branch + 1; }

  let direct_call_of_indirect_known_arity t =
    { t with direct_call_of_indirect = t.direct_call_of_indirect + 1; }

  let direct_call_of_indirect_unknown_arity t =
    { t with direct_call_of_indirect = t.direct_call_of_indirect + 1; }

  let requested_inline denv t ~cost_metrics_of =
    let size = size denv cost_metrics_of in
    { t with requested_inline = t.requested_inline + size; }

  let print ppf b =
    Format.fprintf ppf "@[remove_call: %i@ remove_alloc: %i@ \
                        remove_prim: %i@ remove_branch: %i@ \
                        direct: %i@ requested: %i@]"
      b.remove_call
      b.remove_alloc
      b.remove_prim
      b.remove_branch
      b.direct_call_of_indirect
      b.requested_inline

  let evaluate t ~round : int =
    benefit_factor *
      (t.remove_call * (cost !Clflags.inline_call_cost ~round)
       + t.remove_alloc * (cost !Clflags.inline_alloc_cost ~round)
       + t.remove_prim * (cost !Clflags.inline_prim_cost ~round)
       + t.remove_branch * (cost !Clflags.inline_branch_cost ~round)
       + (t.direct_call_of_indirect
         * (cost !Clflags.inline_indirect_cost ~round)))
    + t.requested_inline

  let (+) t1 t2 = {
    remove_call = t1.remove_call + t2.remove_call;
    remove_alloc = t1.remove_alloc + t2.remove_alloc;
    remove_prim = t1.remove_prim + t2.remove_prim;
    remove_branch = t1.remove_branch + t2.remove_branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect + t2.direct_call_of_indirect;
    requested_inline = t1.requested_inline + t2.requested_inline;
  }

(*
  let (-) t1 t2 = {
    remove_call = t1.remove_call - t2.remove_call;
    remove_alloc = t1.remove_alloc - t2.remove_alloc;
    remove_prim = t1.remove_prim - t2.remove_prim;
    remove_branch = t1.remove_branch - t2.remove_branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect - t2.direct_call_of_indirect;
    requested_inline = t1.requested_inline - t2.requested_inline;
  }
*)

  let max ~round t1 t2 =
    let c1 = evaluate ~round t1 in
    let c2 = evaluate ~round t2 in
    if c1 > c2 then t1 else t2

(*
  let add_code lam b =
    b - (remove_code lam zero)

  let add_code_named lam b =
    b - (remove_code_named lam zero)
*)
end

let scale_inline_threshold_by = 8

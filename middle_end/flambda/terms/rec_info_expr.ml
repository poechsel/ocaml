(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Initial
  | Var of Depth_variable.t
  | Succ of t
  | Unroll_to of int * t

let initial = Initial
let var dv = Var dv
let succ t = Succ t
let unroll_to unroll_depth t = Unroll_to (unroll_depth, t)

let is_obviously_initial = function
  | Initial -> true
  | _ -> false

let rec print ppf = function
  | Initial ->
    Format.pp_print_string ppf "0"
  | Var dv ->
    Depth_variable.print ppf dv
  | Succ t ->
    Format.fprintf ppf "@[<hov 1>(succ@ %a)@]" print t
  | Unroll_to (unroll_depth, t) ->
    Format.fprintf ppf "@[<hov 1>(unroll_to@ %d@ %a)@]" unroll_depth print t

let print_with_cache ~cache:_ ppf t = print ppf t

let rec equal t1 t2 =
  match t1, t2 with
  | Initial, Initial -> true
  | Var dv1, Var dv2 ->
    Depth_variable.equal dv1 dv2
  | Succ t1, Succ t2 ->
    equal t1 t2
  | Unroll_to (unroll_depth1, t1), Unroll_to (unroll_depth2, t2) ->
    unroll_depth1 = unroll_depth2 && equal t1 t2
  | (Initial | Var _ | Succ _ | Unroll_to _), _ -> false

let rec apply_renaming t perm =
  match t with
  | Initial -> Initial
  | Var dv ->
    Var (Renaming.apply_depth_variable perm dv)
  | Succ t ->
    Succ (apply_renaming t perm)
  | Unroll_to (unroll_depth, t) ->
    Unroll_to (unroll_depth, apply_renaming t perm)

let rec free_names = function
  | Initial -> Name_occurrences.empty
  | Var dv -> Name_occurrences.singleton_depth_variable dv
  | Succ t
  | Unroll_to (_, t) -> free_names t

let invariant _ _ = ()

let all_ids_for_export _ =
  (* Depth variables don't use the integer id system *)
  Ids_for_export.empty

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

type t = {
  call : int;
  alloc : int;
  prim : int;
  branch : int;
  (* CR-someday pchambart: branch_benefit : t list; *)
  direct_call_of_indirect : int;
  requested_inline : int;
  (* Benefit to compensate the size of functions marked for inlining *)
}

let zero = {
  call = 0;
  alloc = 0;
  prim = 0;
  branch = 0;
  direct_call_of_indirect = 0;
  requested_inline = 0;
}

let call t = { t with call = t.call + 1; }
let alloc ~count t = { t with alloc = t.alloc + count; }

type classify_prim =
  | Is_alloc
  | Is_prim

let prim ~(prim : Flambda_primitive.t) t =
  let type_ =
    match prim with
    | Unary (prim, _) -> begin match prim with
      | Duplicate_block _ | Duplicate_array _ | Box_number _ | Unbox_number _ ->
        Is_alloc
      | _ -> Is_prim
    end
    | Binary (_prim, _, _) -> Is_prim
    | Ternary (_prim, _, _, _) -> Is_prim
    | Variadic (prim, _) -> begin match prim with
      | Make_block _ | Make_array _ -> Is_alloc
    end
  in
  if type_ = Is_alloc then
    { t with prim = t.alloc + 1; }
  else
    { t with prim = t.prim + 1; }

let branch ~count t = { t with branch = t.branch + count; }
let direct_call_of_indirect t =
  { t with direct_call_of_indirect = t.direct_call_of_indirect + 1; }
let requested_inline ~cost_metrics_of t =
  let cost_metrics = Cost_metrics.to_int cost_metrics_of in
  { t with requested_inline = t.requested_inline + cost_metrics; }

let print ppf b =
  Format.fprintf ppf "@[call: %i@ alloc: %i@ \
                      prim: %i@ branch: %i@ \
                      direct: %i@ requested: %i@]"
    b.call
    b.alloc
    b.prim
    b.branch
    b.direct_call_of_indirect
    b.requested_inline

let (+) t1 t2 = {
  call = t1.call + t2.call;
  alloc = t1.alloc + t2.alloc;
  prim = t1.prim + t2.prim;
  branch = t1.branch + t2.branch;
  direct_call_of_indirect =
    t1.direct_call_of_indirect + t2.direct_call_of_indirect;
  requested_inline = t1.requested_inline + t2.requested_inline;
}

let (-) t1 t2 = {
  call = t1.call - t2.call;
  alloc = t1.alloc - t2.alloc;
  prim = t1.prim - t2.prim;
  branch = t1.branch - t2.branch;
  direct_call_of_indirect =
    t1.direct_call_of_indirect - t2.direct_call_of_indirect;
  requested_inline = t1.requested_inline - t2.requested_inline;
}


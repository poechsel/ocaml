(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Args = struct
  type t = {
    max_inlining_depth : int;
    call_cost : float;
    alloc_cost : float;
    prim_cost : float;
    branch_cost : float;
    indirect_cost : float;
    small_function_size : int;
    large_function_size : int;
    threshold : float;
  }

  let print ppf t =
    Format.fprintf ppf
      "@[<hov 1>(@ \
       @[<hov 1]>(max_inlining_depth@ %d)@]@ \
       @[<hov 1]>(call_cost@ %f)@]@ \
       @[<hov 1]>(alloc_cost@ %f)@]@ \
       @[<hov 1]>(prim_cost@ %f)@]@ \
       @[<hov 1]>(branch_cost@ %f)@]@ \
       @[<hov 1]>(indirect_cost@ %f)@]@ \
       @[<hov 1]>(small_function_size@ %d)@]@ \
       @[<hov 1]>(large_function_size@ %d)@]@ \
       @[<hov 1]>(threshold@ %f)@]\
      )@]"
      t.max_inlining_depth
      t.call_cost
      t.alloc_cost
      t.prim_cost
      t.branch_cost
      t.indirect_cost
      t.small_function_size
      t.large_function_size
      t.threshold

  let equal t1 t2 =
    t1.max_inlining_depth = t2.max_inlining_depth
    && Float.compare t1.call_cost t2.call_cost = 0
    && Float.compare t1.alloc_cost t2.alloc_cost = 0
    && Float.compare t1.prim_cost t2.prim_cost = 0
    && Float.compare t1.branch_cost t2.branch_cost = 0
    && Float.compare t1.indirect_cost t2.indirect_cost = 0
    && t1.small_function_size = t2.small_function_size
    && t1.large_function_size = t2.large_function_size
    && Float.compare t1.threshold t2.threshold = 0

  let (<=) t1 t2 =
    t1.max_inlining_depth <= t2.max_inlining_depth
    && Float.compare t1.call_cost t2.call_cost <= 0
    && Float.compare t1.alloc_cost t2.alloc_cost <= 0
    && Float.compare t1.prim_cost t2.prim_cost <= 0
    && Float.compare t1.branch_cost t2.branch_cost <= 0
    && Float.compare t1.indirect_cost t2.indirect_cost <= 0

  let meet args1 args2 =
    {
      max_inlining_depth = min args1.max_inlining_depth args2.max_inlining_depth;
      call_cost = Float.min args1.call_cost args2.call_cost;
      alloc_cost = Float.min args1.alloc_cost args2.alloc_cost;
      prim_cost = Float.min args1.prim_cost args2.prim_cost;
      branch_cost = Float.min args1.branch_cost args2.branch_cost;
      indirect_cost = Float.min args1.indirect_cost args2.indirect_cost;
      small_function_size = min args1.small_function_size args2.small_function_size;
      large_function_size = min args1.large_function_size args2.large_function_size;
      threshold = Float.min args1.threshold args2.threshold
    }


  let cost_i (flag : Clflags.Int_arg_helper.parsed) ~round =
    Clflags.Int_arg_helper.get ~key:round flag

  let cost_f (flag : Clflags.Float_arg_helper.parsed) ~round =
    Clflags.Float_arg_helper.get ~key:round flag

  let create ~round = {
    max_inlining_depth = !Clflags.Flambda.Expert.max_inlining_depth;
    call_cost = cost_f !Clflags.inline_call_cost ~round;
    alloc_cost = cost_f !Clflags.inline_alloc_cost ~round;
    prim_cost = cost_f !Clflags.inline_prim_cost ~round;
    branch_cost = cost_f !Clflags.inline_branch_cost ~round;
    indirect_cost = cost_f !Clflags.inline_indirect_cost ~round;
    small_function_size = cost_i !Clflags.inline_small_function_size ~round;
    large_function_size = cost_i !Clflags.inline_large_function_size ~round;
    threshold = cost_f !Clflags.inline_threshold ~round;
  }
end

type t = Args.t Or_unknown.t

let unknown = Or_unknown.Unknown

let print ppf = Or_unknown.print Args.print ppf

let max_inlining_depth = function
  | Or_unknown.Unknown -> assert false
  | Or_unknown.Known Args.{ max_inlining_depth; _ } ->
    max_inlining_depth

let call_cost = function
  | Or_unknown.Unknown -> assert false
  | Or_unknown.Known Args.{ call_cost; _ } ->
    call_cost

let alloc_cost = function
  | Or_unknown.Unknown -> assert false
  | Or_unknown.Known Args.{ alloc_cost; _ } ->
    alloc_cost

let prim_cost = function
  | Or_unknown.Unknown -> assert false
  | Or_unknown.Known Args.{ prim_cost; _ } ->
    prim_cost

let branch_cost = function
  | Or_unknown.Unknown -> assert false
  | Or_unknown.Known Args.{ branch_cost; _ } ->
    branch_cost

let indirect_cost = function
  | Or_unknown.Unknown -> assert false
  | Or_unknown.Known Args.{ indirect_cost; _ } ->
    indirect_cost

let small_function_size = function
  | Or_unknown.Unknown -> assert false
  | Or_unknown.Known Args.{ small_function_size; _ } ->
    small_function_size

let large_function_size = function
  | Or_unknown.Unknown -> assert false
  | Or_unknown.Known Args.{ large_function_size; _ } ->
    large_function_size

let threshold = function
  | Or_unknown.Unknown -> assert false
  | Or_unknown.Known Args.{ threshold; _ } ->
    threshold

let meet (t1 : _ Or_unknown.t) (t2 : _ Or_unknown.t) : t =
  match t1, t2 with
  | Unknown, Unknown -> Unknown
  | Known _, Unknown -> t1
  | Unknown, Known _ -> t2
  | Known args1, Known args2 ->
    if Args.(<=) args1 args2 then
      t1
    else if Args.(<=) args2 args1 then
      t2
    else
      Known (Args.meet args1 args2)

let create ~round : t = Known (Args.create ~round)

let equal t1 t2 = Or_unknown.equal Args.equal t1 t2

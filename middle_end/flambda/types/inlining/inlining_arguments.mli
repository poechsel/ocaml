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

(** Set of all arguments related to inlining. *)

type t

val unknown : t

val create : round:int -> t

val print : Format.formatter -> t -> unit

(* [meet A B] constructs a set of argument that inline at most as strongly as [A]
   and [B] would *)
val meet : t -> t -> t

val equal : t -> t -> bool

val max_inlining_depth : t -> int

val call_cost : t -> float

val alloc_cost : t -> float

val prim_cost : t -> float

val branch_cost : t -> float

val indirect_cost : t -> float

val small_function_size : t -> int

val large_function_size : t -> int

val threshold : t -> float

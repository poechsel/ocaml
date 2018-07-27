(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Pierre Oechsel, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Unboxing : sig
  type t = {
    unbox_specialised_args : bool;
    unbox_free_vars_of_closures : bool;
    unbox_closures : bool;
    unbox_closures_factor : int;
    remove_unused_arguments : bool;
  }
  type u

  val extract : t -> u

  val get : unit -> t
end

module Inlining : sig
  type t

  val inline_call_cost : t -> int

  val inline_alloc_cost : t -> int

  val inline_prim_cost : t -> int

  val inline_branch_cost : t -> int

  val inline_indirect_cost : t -> int

  val inline_lifting_benefit : t -> int

  val inline_branch_factor : t -> float

  val inline_max_depth : t -> int

  val inline_max_speculation_depth : t -> int

  val inline_max_unroll : t -> int

  val inline_threshold : t -> int

  val inline_toplevel_threshold : t -> int

  (* get the [inlining_arguments] structure corresponding
      to a given round *)
  val get : int -> t

  (* get an [inlining_arguments] struct filled with the
      maximum values across all round.
      As we are forcing them to be increasing over rounds, this is equivalent to be
      returning the arguments of the last round *)
  val get_max : unit -> t

  (* Merge two inlining arguments structures:
      Keep the weaker version of each of their attributes *)
  val merge : t -> t -> t

  (* Check the integrity of inlining arguments, ie that they are in increasing
     order when the round grows up.
     If it's not the case, output a warning and restore monotonicity *)
  val ensure_integrity : unit -> unit
end

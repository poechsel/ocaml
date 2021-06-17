(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Ilambda: halfway between Lambda and Flambda. In CPS but without closures.

    This language is used only as an internal language for communication between
    the CPS and closure conversion passes. We don't do any transformations on
    the language, so there is little abstraction, and no features such as
    advanced treatment of name binding.

    Flambda expressions augment Ilambda expressions by adding constructs for:
    - the construction and manipulation of closures; and
    - accessing constants that have been lifted to static data.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: We need to stop primitives having control flow effects, e.g.
   asynchronous exceptions from finalisers, really.
   Although there's still the problem with signal handlers and Ctrl+C.  Maybe
   we should have allocation as a distinguished "named". *)

type simple =
  | Var of Ident.t
  | Const of Lambda.structured_constant

type exn_continuation =
  { exn_handler : Continuation.t;
    extra_args : (simple * Lambda.value_kind) list;
  }

type trap_action =
  | Push of { exn_handler : Continuation.t; }
  | Pop of { exn_handler : Continuation.t; }

type user_visible =
  | User_visible
  | Not_user_visible

type named =
  | Simple of simple
  | Prim of {
      prim : Lambda.primitive;
      args : simple list;
      loc : Lambda.scoped_location;
      exn_continuation : exn_continuation option;
    }
    (** Set [exn_continuation] to [None] iff the given primitive can never
        raise. *)

type apply_kind =
  | Function
  | Method of { kind : Lambda.meth_kind; obj : simple; }

type apply = {
  kind : apply_kind;
  func : Ident.t;
  args : simple list;
  continuation : Continuation.t;
  exn_continuation : exn_continuation;
  loc : Lambda.scoped_location;
  tailcall : Lambda.tailcall_attribute;
  inlined : Lambda.inline_attribute;
  specialised : Lambda.specialise_attribute;
}

type switch = {
  numconsts : int;
  consts : (int * Continuation.t * trap_action option * (simple list)) list;
  failaction : (Continuation.t * trap_action option * (simple list)) option;
}

val print_named : Format.formatter -> named -> unit

val contains_functions : Lambda.lambda -> bool

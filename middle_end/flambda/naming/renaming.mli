(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Handling of permutations and import freshening upon all kinds of
    bindable names and other identifiers (e.g. constants).

    Unlike [Name_occurrences] this module does not segregate names according
    to where they occur (e.g. in terms or in types). *)

[@@@ocaml.warning "+a-30-40-41-42"]

type t

val empty : t

val print : Format.formatter -> t -> unit

val has_no_action : t -> bool

val of_import_map : Ids_for_export.Import_map.t -> t

(** Note that [compose] is not commutative on the permutation component.
    The permutation in the result of [compose ~second ~first] is that
    permutation acting initially like [first] then subsequently like [second].
*)
val compose : second:t -> first:t -> t

val add_variable : t -> Variable.t -> Variable.t -> t

val add_fresh_variable : t -> Variable.t -> guaranteed_fresh:Variable.t -> t

val apply_variable : t -> Variable.t -> Variable.t

val apply_variable_set : t -> Variable.Set.t -> Variable.Set.t

val add_symbol : t -> Symbol.t -> Symbol.t -> t

val add_fresh_symbol : t -> Symbol.t -> guaranteed_fresh:Symbol.t -> t

val apply_symbol : t -> Symbol.t -> Symbol.t

val apply_symbol_set : t -> Symbol.Set.t -> Symbol.Set.t

val apply_name : t -> Name.t -> Name.t

val add_continuation : t -> Continuation.t -> Continuation.t -> t

val add_fresh_continuation
   : t
  -> Continuation.t
  -> guaranteed_fresh:Continuation.t
  -> t

val apply_continuation : t -> Continuation.t -> Continuation.t

val add_code_id : t -> Code_id.t -> Code_id.t -> t

val add_fresh_code_id
   : t
  -> Code_id.t
  -> guaranteed_fresh:Code_id.t
  -> t

val apply_code_id : t -> Code_id.t -> Code_id.t

val apply_simple : t -> Reg_width_things.Simple.t -> Reg_width_things.Simple.t

(* See note in ids_for_export.ml. *)
val closure_var_is_used : t -> Var_within_closure.t -> bool

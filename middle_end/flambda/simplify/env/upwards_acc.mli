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

type t

(** Print a upwards accumulator to a formatter. *)
val print : Format.formatter -> t -> unit

val create : Upwards_env.t -> Downwards_acc.t -> t

val creation_dacc : t -> Downwards_acc.t

(** Extract the environment component of the given upwards accumulator. *)
val uenv : t -> Upwards_env.t

val size : t -> Flambda.Code_size.t

val code_age_relation : t -> Code_age_relation.t

(** Return the lifted constants that still need to be placed (i.e. have
    [Let]-expressions made for them) on the upwards traversal. *)
val lifted_constants : t -> Lifted_constant_state.t

val add_outermost_lifted_constant : t -> Lifted_constant.t -> t

(** Replace the accumulator of lifted constants returned by
    [lifted_constants]. *)
val with_lifted_constants : t -> Lifted_constant_state.t -> t

val no_lifted_constants : t -> bool

(** Map the environment component of the given upwards accumulator. *)
val map_uenv
   : t
  -> f:(Upwards_env.t
    -> Upwards_env.t)
  -> t

(** Replace the environment component of the given upwards accumulator. *)
val with_uenv : t -> Upwards_env.t -> t

val remember_code_for_cmx : t -> Flambda.Code.t Code_id.Map.t -> t

val all_code : t -> Exported_code.t

val shareable_constants : t -> Symbol.t Flambda.Static_const.Map.t

val name_occurrences : t -> Name_occurrences.t

val with_name_occurrences : t -> name_occurrences:Name_occurrences.t -> t

val clear_name_occurrences : t -> t

val add_free_names : t -> Name_occurrences.t -> t

val used_closure_vars : t -> Name_occurrences.t

val remove_all_occurrences_of_free_names : t -> Name_occurrences.t -> t

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

(* Computes an approximatation for the code size corresponding to flambda terms.*)

type t

val expr_size : find_code:(Code_id.t -> Code.t) -> Expr.t -> t

val of_int : int -> t
val to_int : t -> int
val (+) : t -> t -> t
val smaller : t -> than:t -> bool
val print : Format.formatter -> t -> unit

val prim : Flambda_primitive.t -> t
val simple : Simple.t -> t
val set_of_closures : find_code_size:(Code_id.t -> Code_size.t Or_unknown.t) -> Set_of_closures.t -> t

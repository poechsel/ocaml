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

val zero: t
val print: Format.formatter -> t -> unit

val (+): t -> t -> t
val (-): t -> t -> t

val call: t -> t
val alloc: count:int -> t -> t
val prim: prim:Flambda_primitive.t -> t -> t
val branch: count:int -> t -> t
val direct_call_of_indirect: t -> t
val requested_inline: size_of:Code_size.t -> t -> t


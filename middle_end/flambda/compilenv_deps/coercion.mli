(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = private
  | Id
  | Non_id of {
      from_depth : int;
      to_depth : int;
    }

val change_depth : from:int -> to_:int -> t

val id : t

val is_id : t -> bool

val inverse : t -> t

val compose : t -> then_:t -> t option

val compose_exn : t -> then_:t -> t

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val hash : t -> int

val apply_to_rec_info : t -> Rec_info.t -> Rec_info.t

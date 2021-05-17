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

(** A [Depth_variable] equipped with operations that mean it can be used in
    binding position within a [Name_abstraction] value. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = Depth_variable.t

include Bindable.S
  with type t := t
  with module Set = Depth_variable.Set
  with module Map = Depth_variable.Map

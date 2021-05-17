(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t
type exported

include Identifiable.S with type t := t

val create : string -> t

val name : t -> string

val name_stamp : t -> int

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

val rename : t -> t

val export : t -> exported

val import : exported -> t

val map_compilation_unit :
  (Compilation_unit.t -> Compilation_unit.t) -> exported -> exported

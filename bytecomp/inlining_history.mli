(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = node list

and node =
  | Module of string * Debuginfo.t
  | Closure of string * Debuginfo.t
  | Call of string * Lambda.DebugNames.t * Debuginfo.t * t option
  | Inlined
  | Specialised of string

val create : unit -> t

val compare_node : node -> node -> int

val compare : t -> t -> int

val print_node : Format.formatter -> node -> unit

val print : Format.formatter -> t -> unit

val add : t -> t -> t

val strip_history : t -> t

val note_entering_closure
  : t
  -> name:string
  -> dbg:Debuginfo.t
  -> t

val note_entering_call
  : t
  -> name:string
  -> dbg_name:Lambda.DebugNames.t
  -> dbg:Debuginfo.t
  -> absolute_inlining_history:t option
  -> t

val note_entering_inlined : t -> t
val note_entering_specialised : t -> name:string -> t

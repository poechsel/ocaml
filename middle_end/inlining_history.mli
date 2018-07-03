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

type class_name_type =
  | ObjInit
  | NewInit
  | ClassInit
  | ClassRebind
  | EnvInit

type name =
  | Function of string
  | Functor of string
  | Class of string * class_name_type
  | Anonymous
  | Coerce
  | Method of string * string

type t = node list


and node =
  | Module of string * Debuginfo.t * string list
  | Closure of name * Debuginfo.t
  | Call of string * t * Debuginfo.t * t option
  | Inlined
  | Specialised of string

val create : unit -> t

val empty : t

val compare_node : node -> node -> int

val compare : t -> t -> int

val print_node : Format.formatter -> node -> unit

val print : Format.formatter -> t -> unit

val add : t -> t -> t

val strip_history : t -> t

val string_of_name : name -> string

val note_entering_closure
  : t
  -> name:name
  -> dbg:Debuginfo.t
  -> t

val note_entering_call
  : t
  -> name:string
  -> dbg_name:t
  -> dbg:Debuginfo.t
  -> absolute_inlining_history:t option
  -> t

val note_entering_inlined : t -> t
val note_entering_specialised : t -> name:string -> t

val add_fn_def
  : name:name
  -> loc:Location.t
  -> path:t
  -> t

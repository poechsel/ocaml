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
  | Call of path * Debuginfo.t * path
  | Inlined
  | Specialised of string

and path
and atom

val create : unit -> t

val empty : t

val compare : path -> path -> int

val print_atom :
  Format.formatter
  -> atom
  -> unit

val print :
  Format.formatter
  -> path
  -> unit

val uid_of_path :
  path
  -> string

val add : t -> t -> t

val string_of_name : name -> string

val empty_path : path

val note_entering_closure
  : t
  -> name:name
  -> dbg:Debuginfo.t
  -> t

val note_entering_call
  : t
  -> dbg_name:path option
  -> dbg:Debuginfo.t
  -> absolute_inlining_history:t
  -> t

val note_entering_inlined : t -> t
val note_entering_specialised : t -> name:string -> t

val add_fn_def
  : name:name
  -> loc:Location.t
  -> path:t
  -> t

val history_to_path:
  t -> path

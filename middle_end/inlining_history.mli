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
  | SpecialisedFunction of name
  | Functor of string
  | Class of string * class_name_type
  | Anonymous
  | Coerce
  | Method of string * string

val print_name :
  Format.formatter
  -> name
  -> unit


module Path : sig
  type t = atom list
  and atom =
    | Module of string * Debuginfo.item
    | Closure of name * Debuginfo.item
    | Call of t * Debuginfo.item
    | File of string option * string
    | Inlined
    | Specialised
    | SpecialisedCall

  val compare_atom : atom -> atom -> int

  val compare : t -> t -> int

  val add_import_atoms : string -> t -> t

  val print_atom :
    Format.formatter
    -> atom
    -> unit

  val print :
    Format.formatter
    -> t
    -> unit

  val empty : t

  val to_uid : t -> string

  (* "compress" a path knowing an other path : returns the second path
     with the prefix of the two paths removed. If the result is empty,
     returns a path made of the last atom *)
  val get_compressed_path : t -> t -> t

  (* remove AInlined, ASpecialised and ASpecialisedCall atoms
     from a path *)
  val strip_call_attributes : t -> t
end

module History : sig
  type atom =
    | Module of string * Debuginfo.item
    | Closure of name * Debuginfo.item
    | Call of Path.t * Debuginfo.item * Path.t
    | Inlined
    | Specialised
    | SpecialisedCall
  type t = atom list

  val create : unit -> t

  val empty : t

  val extract_def_name : t -> name
end

val node_to_atom : History.atom -> Path.atom

val add : History.t -> History.t -> History.t

val string_of_name : name -> string


val note_entering_call
  : History.t
  -> dbg_name:Path.t
  -> dbg:Debuginfo.item
  -> absolute_inlining_history:History.t
  -> History.t

val add_fn_def
  : name:name
  -> loc:Location.t
  -> path:History.t
  -> History.t

val add_mod_def
  : id:Ident.t
  -> loc:Location.t
  -> path:History.t
  -> History.t

val history_to_path: History.t -> Path.t

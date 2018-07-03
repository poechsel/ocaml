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

let create () = []
let empty = []

(* beware, the following are more to check equality than true
   order *)
let rec compare_node a b =
  match a, b with
  | Inlined, Inlined ->
    0
  | Module (a, b, l), Module(a', b', l') ->
    if a = a' then 0
    else let c = Debuginfo.compare b b' in
      if c <> 0 then c
      else Pervasives.compare l l'
  | Closure(a, b), Closure(a', b') ->
    if a = a' then 0 else
      Debuginfo.compare b b'
  | Call(a, _, b, p), Call(a', _, b', p') ->
    if a = a' then 0 else
      let c = Debuginfo.compare b b' in
      if c <> 0 then c else begin
        match p, p' with
        | None, None -> 0
        | Some l, Some l' ->
          compare l l'
        | _ -> -1
      end
  | Specialised(a), Specialised(a') ->
    Pervasives.compare a a'
  | _ -> -1

and compare l l' =
  let c = List.compare_lengths l l' in
  if c <> 0 then c else
    List.fold_left2 (fun p e e' ->
      if p <> 0 then p
      else compare_node e e')
      0 l l'

let strip_history hist =
  List.map (function | Call(a, c, b, _) -> Call (a, c, b, None)
                     | x -> x) hist


let print_name ppf name =
  match name with
  | Function n ->
    Format.fprintf ppf "%s" n
  | Functor n ->
    Format.fprintf ppf "functor %s" n
  | Anonymous ->
    Format.fprintf ppf "anonymous"
  | Coerce ->
    Format.fprintf ppf "coercion"
  | Method(a, b) ->
    Format.fprintf ppf "method %s of %s" b a
  | Class(n, w) ->
    let w =
      match w with
      | ObjInit ->
        "object init"
      | NewInit ->
        "new init"
      | ClassInit ->
        "class init"
      | ClassRebind ->
        "class rebind"
      | EnvInit ->
        "env init"
    in
    Format.fprintf ppf "%s of %s" w n

let string_of_name name =
  Format.asprintf "%a" print_name name


let print_node ppf x =
  match x with
  | Inlined ->
    Format.fprintf ppf "inlined "
  | Call (c, _, _, _) ->
    Format.fprintf ppf "call(%s) " c
  | Closure (c, _) ->
    Format.fprintf ppf "closure(%a) " print_name c
  | Module (c, _, params) ->
    let params =
      match params with
      | [] -> ""
      | _ -> "(" ^ String.concat ", " params ^ ")"
    in
    Format.fprintf ppf "module(%s%s) " c params
  | _ ->
    Format.fprintf ppf "specialise "

let print ppf l =
  List.iter (print_node ppf) l

let add a b =
  (* order is important. If b= [1; 2] and a = [3;4;5],
     we want the result to be [1;2;3;4;5] *)
  a @ b

let note_entering_closure t ~name ~dbg =
  if not !Clflags.inlining_report then t
  else
    match t with
    | [] | (Closure _ | Inlined | Specialised _ | Module _)  :: _->
      (Closure (name, dbg)) :: t
    | (Call _) :: _ ->
      Misc.fatal_errorf "note_entering_closure: unexpected Call node"

(* CR-someday lwhite: since calls do not have a unique id it is possible
   some calls will end up sharing nodes. *)
let note_entering_call t ~name ~dbg_name ~dbg
      ~absolute_inlining_history =
  let dbg_name =
    match dbg_name with
    | None ->
      (*CR poechsel: examine why puttin an assert false here fails*)
      empty
    | Some x -> x
  in
  (Call (name, dbg_name, dbg, absolute_inlining_history)) :: t

let note_entering_inlined t =
  if not !Clflags.inlining_report then t
  else
    match t with
    | [] | (Closure _ | Inlined | Specialised _ | Module _) :: _->
      Misc.fatal_errorf "anote_entering_inlined: missing Call node"
    | (Call _) :: _ -> Inlined :: t

let note_entering_specialised t ~name =
  if not !Clflags.inlining_report then t
  else
    match t with
    | [] | (Closure _ | Inlined | Specialised _ | Module _) :: _ ->
      Misc.fatal_errorf "bnote_entering_specialised: missing Call node"
    | (Call _) :: _ -> Specialised name :: t

let add_fn_def ~name ~loc ~path =
  Closure(name, Debuginfo.from_location loc) :: path

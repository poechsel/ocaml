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

(* shorten representation *)
and path = atom list
and atom =
  | AModule of string * Debuginfo.t
  | AClosure of name * Debuginfo.t
  | ACall of path * Debuginfo.t
  | AInlined
  | ASpecialised of string

let create () = []
let empty = []

let history_to_path (history : t) : path =
  history
  |> List.map (function
    | Module(s, d, _) -> AModule(s, d)
    | Closure(n, d) -> AClosure(n, d)
    | Call(p, d, _) -> ACall(p, d)
    | Inlined -> AInlined
    | Specialised s -> ASpecialised s
  )
|> List.rev

(* beware, the following are more to check equality than true
   order *)
let rec compare_atom a b =
  match a, b with
  | AInlined, AInlined ->
    0
  | AModule (a, b), AModule(a', b') ->
    if a = a' then 0
    else let c = Debuginfo.compare b b' in
      c
  | AClosure(a, b), AClosure(a', b') ->
    if a = a' then 0 else
      Debuginfo.compare b b'
  | ACall(a, b), ACall(a', b') ->
    let c = compare a a' in
    if c <> 0 then c else
      let c = Debuginfo.compare b b' in
      c
  | ASpecialised(a), ASpecialised(a') ->
    Pervasives.compare a a'
  | _ -> -1

and compare l l' =
  let c = List.compare_lengths l l' in
  if c <> 0 then c else
    List.fold_left2 (fun p e e' ->
      if p <> 0 then p
      else compare_atom e e')
      0 l l'

let empty_path = []

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

let rec uid_of_path h =
  Marshal.to_bytes h []
  |> Digest.bytes
  |> Digest.to_hex

and print_atom ppf x =
  match x with
  | AInlined ->
    Format.fprintf ppf " inlined "
  | ACall (c, _) ->
    Format.fprintf ppf "%a" print c
  | AClosure (c, _) ->
    Format.fprintf ppf "%a" print_name c
  | AModule (c, _) ->
    let params = ""
      (*match params with
      | [] -> ""
      | _ -> "(" ^ String.concat ", " params ^ ")"
      *)in
    Format.fprintf ppf "%s%s." c params
  | _ ->
    Format.fprintf ppf " specialise "
and print ppf l =
  List.iter (print_atom ppf) l


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
let note_entering_call t ~dbg_name ~dbg
      ~absolute_inlining_history =
  let dbg_name =
    match dbg_name with
    | None ->
      (*CR poechsel: examine why puttin an assert false here fails*)
      empty
    | Some x -> x
  in
  let absolute_inlining_history =
    (* adding a placeholder call node to represent this call inside the
       absolute history. Its absolute path does not matters as it will
       be stripped during the conversion *)
    Call(dbg_name, dbg, empty) :: absolute_inlining_history
    |> history_to_path
  in
  (Call (dbg_name, dbg, absolute_inlining_history)) :: t

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
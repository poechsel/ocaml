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


type t = node list

and node =
  | Module of string * Debuginfo.item
  | Closure of name * Debuginfo.item
  | Call of path * Debuginfo.item * path
  | Inlined
  | Specialised
  | SpecialisedCall

(* shorten representation *)
and path = atom list
and atom =
  | AModule of string * Debuginfo.item
  | AClosure of name * Debuginfo.item
  | ACall of path * Debuginfo.item
  | AFile of string option * string
  | AInlined
  | ASpecialised
  | ASpecialisedCall

let create () = []
let empty = []

let node_to_atom = function
    | Module(s, d) -> AModule(s, d)
    | Closure(n, d) -> AClosure(n, d)
    | Call(p, d, _) -> ACall(p, d)
    | Inlined -> AInlined
    | Specialised -> ASpecialised
    | SpecialisedCall -> ASpecialisedCall

let history_to_path (history : t) : path =
  history
  |> List.map node_to_atom
  |> List.rev

let path_add_import_atoms modname path =
  let filename =
    try
      Some (Misc.find_in_path_uncap
              !Config.load_path
              (modname ^ ".inlining.org"))
    with Not_found ->
      None
  in
  AFile(filename, modname) :: path


let rec compare_name a b =
  let index x =
    match x with
    | Function _ -> 0
    | SpecialisedFunction _ -> 1
    | Functor _ -> 2
    | Class _ -> 3
    | Anonymous -> 4
    | Coerce -> 5
    | Method _ -> 6
  in
  let index_a = index a in
  let index_b = index b in
  if index_a <> index_b then
    if index_a < index_b then -1 else 1
  else
    begin match a, b with
    | Function a, Function b ->
      String.compare a b
    | SpecialisedFunction a, SpecialisedFunction b ->
      compare_name a b
    | Functor a, Functor b ->
      String.compare a b
    | Class(a, t), Class(a', t') ->
      let index = function
        | ObjInit -> 0
        | NewInit -> 1
        | ClassInit -> 2
        | ClassRebind -> 3
        | EnvInit -> 4
      in
      let c = String.compare a a' in
      if c <> 0 then c else
        let it = index t in
        let it' = index t' in
        if it = it' then 0
        else if it < it' then -1
        else 1
    | Anonymous, Anonymous ->
      0
    | Coerce,  Coerce ->
      0
    | Method (a, b), Method (a', b') ->
      let c = String.compare a a' in
      if c <> 0 then c else
        String.compare b b'
    | _ -> assert false
    end

let extract_debug_info atom =
  match atom with
  | AClosure(_, dbg)
  | AModule(_, dbg)
  | ACall(_, dbg) ->
    Some dbg
  | _ ->
    None

let rec compare_atom a b =
  let c =
    match extract_debug_info a, extract_debug_info b with
    | Some a, Some b -> Debuginfo.compare [a] [b]
    | _ -> 0
  in
  if c <> 0 then c
  else
  let index = function
    | AFile _ -> 0
    | AClosure _ -> 1
    | AModule _ -> 2
    | ACall _ -> 3
    | ASpecialised -> 4
    | ASpecialisedCall -> 5
    | AInlined -> 6
  in
  let index_a = index a in
  let index_b = index b in
  if index_a <> index_b then
    if index_a < index_b then -1 else 1
  else
  match a, b with
  | AFile (_, b), AFile (_, b') ->
    String.compare b b'
  | AInlined, AInlined ->
    0
  | AModule (a, b), AModule(a', b') ->
    let c = Debuginfo.compare [b] [b'] in
    if c <> 0 then c
    else String.compare a a'
  | AClosure(a, b), AClosure(a', b') ->
    let c = Debuginfo.compare [b] [b'] in
    if c <> 0 then c else
      compare_name a a'
  | ACall(a, b), ACall(a', b') ->
    let c = compare a a' in
    if c <> 0 then c else
      let c = Debuginfo.compare [b] [b'] in
      c
  | ASpecialised, ASpecialised ->
    0
  | ASpecialisedCall, ASpecialisedCall ->
    0
  | _ -> assert false

and compare l l' =
  let c = List.compare_lengths l l' in
  if c <> 0 then c else
    List.fold_left2 (fun p e e' ->
      if p <> 0 then p
      else compare_atom e e')
      0 l l'

let empty_path = []

let rec print_name ppf name =
  match name with
  | SpecialisedFunction n ->
    Format.fprintf ppf "specialised of %a" print_name n
  | Function n ->
    Format.fprintf ppf "%s" n
  | Functor n ->
    Format.fprintf ppf "functor %s" n
  | Anonymous ->
    Format.fprintf ppf "anonymous"
  | Coerce ->
    Format.fprintf ppf "coercion"
  | Method(a, b) ->
    Format.fprintf ppf "%s#method %s" a b
  | Class(n, w) ->
    let w =
      match w with
      | ObjInit ->
        "object_init"
      | NewInit ->
        "new_init"
      | ClassInit ->
        "class_init"
      | ClassRebind ->
        "class_rebind"
      | EnvInit ->
        "env_init"
    in
    Format.fprintf ppf "%s#%s" n w


let string_of_name name =
  Format.asprintf "%a" print_name name

let rec uid_of_path h =
  let h =
    match h with
        | AFile _ :: h -> h
        | h -> h
  in
  Marshal.to_bytes h []
  |> Digest.bytes
  |> Digest.to_hex

and print_atom ppf x =
  match x with
  | AInlined ->
    Format.fprintf ppf " inlined "
  | ACall (c, _) ->
    Format.fprintf ppf "(%a ) " print c
  | AClosure (c, _) ->
    Format.fprintf ppf "%a " print_name c
  | AModule (c, _)
  | AFile (_, c) ->
    Format.fprintf ppf "%s." c
  | ASpecialised ->
    (* printing nothing because it's already done in the name *)
    Format.fprintf ppf ""
  | ASpecialisedCall ->
    Format.fprintf ppf " specialised call "

and print ppf l =
  List.iter (print_atom ppf) l

let rec get_compressed_path root leaf =
  match root, leaf with
  | atom :: root, atom' :: leaf when
      compare_atom atom atom' = 0 ->
    let r = get_compressed_path root leaf in
    if r = [] then atom' :: empty_path else r
  | _ -> leaf

let strip_call_attributes path =
  List.filter (function
    | AInlined | ASpecialised | ASpecialisedCall -> false
    | _ -> true)
    path

let add a b =
  (* order is important. If a= [1; 2] and b = [3;4;5],
     we want the result to be [1;2;3;4;5] *)
  a @ b

let note_entering_closure t ~name ~dbg =
  if not !Clflags.inlining_report then t
  else
    match t with
    | [] | (Closure _ | Inlined | Specialised | SpecialisedCall | Module _)  :: _->
      (Closure (name, dbg)) :: t
    | (Call _) :: _ ->
      Misc.fatal_errorf "note_entering_closure: unexpected Call node"

(* CR-someday lwhite: since calls do not have a unique id it is possible
   some calls will end up sharing nodes. *)
let note_entering_call t ~dbg_name ~dbg
      ~absolute_inlining_history =
  let absolute_inlining_history =
    (* adding a placeholder call node to represent this call inside the
       absolute history. Its absolute path does not matters as it will
       be stripped during the conversion *)
    Call(dbg_name, dbg, empty) :: absolute_inlining_history
    |> history_to_path
  in
  (Call (dbg_name, dbg, absolute_inlining_history)) :: t

let add_fn_def ~name ~loc ~path =
  Closure(name, Debuginfo.item_from_location loc) :: path

let extract_def_name history =
  match history with
  | Closure (x, _) :: _ ->
    x
  | Specialised :: _
  | _ ->
    assert(false)

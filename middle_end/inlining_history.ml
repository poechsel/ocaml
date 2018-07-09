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



module Path = struct
  (* shorten representation *)
  type t = atom list
  and atom =
    | Module of string * Debuginfo.item
    | Closure of name * Debuginfo.item
    | Call of t * Debuginfo.item
    | File of string option * string
    | Inlined
    | Specialised
    | SpecialisedCall

  let empty = []

let add_import_atoms modname path =
  let filename =
    try
      Some (Misc.find_in_path_uncap
              !Config.load_path
              (modname ^ ".inlining.org"))
    with Not_found ->
      None
  in
  File(filename, modname) :: path


let extract_debug_info atom =
  match atom with
  | Closure(_, dbg)
  | Module(_, dbg)
  | Call(_, dbg) ->
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
    | File _ -> 0
    | Closure _ -> 1
    | Module _ -> 2
    | Call _ -> 3
    | Specialised -> 4
    | SpecialisedCall -> 5
    | Inlined -> 6
  in
  let index_a = index a in
  let index_b = index b in
  if index_a <> index_b then
    if index_a < index_b then -1 else 1
  else
  match a, b with
  | File (_, b), File (_, b') ->
    String.compare b b'
  | Inlined, Inlined ->
    0
  | Module (a, b), Module(a', b') ->
    let c = Debuginfo.compare [b] [b'] in
    if c <> 0 then c
    else String.compare a a'
  | Closure(a, b), Closure(a', b') ->
    let c = Debuginfo.compare [b] [b'] in
    if c <> 0 then c else
      compare_name a a'
  | Call(a, b), Call(a', b') ->
    let c = compare a a' in
    if c <> 0 then c else
      let c = Debuginfo.compare [b] [b'] in
      c
  | Specialised, Specialised ->
    0
  | SpecialisedCall, SpecialisedCall ->
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

let rec to_uid h =
  let h =
    match h with
    | File _ :: h -> h
    | h -> h
  in
  Marshal.to_bytes h []
  |> Digest.bytes
  |> Digest.to_hex

and print_atom ppf x =
  match x with
  | Inlined ->
    Format.fprintf ppf " inlined "
  | Call (c, _) ->
    Format.fprintf ppf "(%a ) " print c
  | Closure (c, _) ->
    Format.fprintf ppf "%a " print_name c
  | Module (c, _)
  | File (_, c) ->
    Format.fprintf ppf "%s." c
  | Specialised ->
    (* printing nothing because it's already done in the name *)
    Format.fprintf ppf ""
  | SpecialisedCall ->
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
    | Inlined | Specialised | SpecialisedCall -> false
    | _ -> true)
    path

end


module History = struct
  type t = atom list
  and atom =
    | Module of string * Debuginfo.item
    | Closure of name * Debuginfo.item
    | Call of Path.t * Debuginfo.item * Path.t
    | Inlined
    | Specialised
    | SpecialisedCall

  let create () = []

  let empty = []

  let extract_def_name history =
  match history with
  | Closure (x, _) :: _ ->
    x
  | Specialised :: _
  | _ ->
    assert(false)

  let remove_most_recent_atom p =
    match p with
    | [] -> []
    | _ :: p -> p

  let insert atom history =
    atom :: history

  let add a b =
    (* order is important. If a= [1; 2] and b = [3;4;5],
       we want the result to be [1;2;3;4;5] *)
    a @ b

end

let node_to_atom (history : History.atom) : Path.atom =
  match history with
    | Module(s, d) -> Module(s, d)
    | Closure(n, d) -> Closure(n, d)
    | Call(p, d, _) -> Call(p, d)
    | Inlined -> Inlined
    | Specialised -> Specialised
    | SpecialisedCall -> SpecialisedCall

let history_to_path (history : History.t) : Path.t =
  history
  |> List.map node_to_atom
  |> List.rev


let note_entering_call t ~dbg_name ~dbg
      ~absolute_inlining_history =
  let absolute_inlining_history =
    (* adding a placeholder call node to represent this call inside the
       absolute history. Its absolute path does not matters as it will
       be stripped during the conversion *)
    History.Call(dbg_name, dbg, History.empty) :: absolute_inlining_history
    |> history_to_path
  in
  (History.Call (dbg_name, dbg, absolute_inlining_history)) :: t

let add_fn_def ~name ~loc ~path =
  History.Closure(name, Debuginfo.item_from_location loc) :: path

let add_mod_def ~id ~loc ~path =
  History.Module(id, Debuginfo.item_from_location loc) :: path

let add_specialise_def ~name ~path =
  History.Closure(SpecialisedFunction name, Debuginfo.none_item)
  :: History.Specialised :: path

let add_specialise_apply ~path =
  History.SpecialisedCall :: path

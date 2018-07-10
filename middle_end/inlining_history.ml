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

let rec print_name ~print_functor ppf name =
  match name with
  | SpecialisedFunction n ->
    Format.fprintf ppf "specialised of %a" (print_name ~print_functor) n
  | Function n ->
    Format.fprintf ppf "%s" n
  | Functor n ->
    if print_functor then
      Format.fprintf ppf "functor %s" n
    else
      Format.fprintf ppf "%s" n
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
  Format.asprintf "%a" (print_name ~print_functor:true) name

module Definition = struct
  type t = atom list
  and atom =
    | Module of string
    | Closure of name * Debuginfo.item
    | File of string

  let empty = []

  let get_last_definition def =
    List.fold_left (fun previous x ->
      match x with
      | Module _ | File _ -> previous
      | Closure(n, x) -> Some (n, x)
    ) None def

  let print_short ppf def =
    let name =
      match get_last_definition def with
      | None -> Anonymous
      | Some (a, _) -> a
    in
    Format.fprintf ppf "%a"
      (print_name ~print_functor:true) name

  let print ppf def =
    let rec aux ~print_functor ppf def =
      match def with
      | [] -> ()
      | Closure(name, _) :: (File _ | Module _ as x) :: r ->
        let is_functor =
          match name with
          | Functor _ -> true
          | _ -> false
        in
        let functor_str =
          if is_functor then "functor "
          else ""
        in
        let print_functor = not is_functor in
        Format.fprintf ppf "%s%a%a" functor_str
          (aux ~print_functor) (x::r)
          (print_name ~print_functor) name
      | Closure(name, _) :: [] ->
        Format.fprintf ppf "%a"
          (print_name ~print_functor) name
      | Closure(name, _) :: r ->
        Format.fprintf ppf "%a defined in %a"
          (print_name ~print_functor) name
          (aux ~print_functor:true) r
      | Module name :: r ->
        Format.fprintf ppf "%a%s." (aux ~print_functor) r name
      | File (name) :: r ->
        Format.fprintf ppf "%a%s." (aux ~print_functor) r name
    in
    Format.fprintf ppf "Call to %a" (aux ~print_functor:true) (List.rev def);
    match (get_last_definition def) with
    | None ->
      Format.fprintf ppf "."
    | Some(_, dbg) ->
      Format.fprintf ppf ", which was defined at %s."
        (Debuginfo.to_string [dbg])
end

module Path = struct
  (* shorten representation *)
  type t = string * path
  and path = atom list
  and atom =
    | Module of string * Debuginfo.item
    | Closure of name * Debuginfo.item
    | Call of t * Debuginfo.item
    | Inlined
    | Specialised
    | SpecialisedCall

  let empty modname = modname, []

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

  and compare (f, l) (f', l') =
    let c = String.compare f f' in
    if c <> 0 then c else
      let c = List.compare_lengths l l' in
      if c <> 0 then c else
        List.fold_left2 (fun p e e' ->
          if p <> 0 then p
          else compare_atom e e')
          0 l l'

  let rec to_uid h =
    Marshal.to_bytes h []
    |> Digest.bytes
    |> Digest.to_hex

  and print_atom ppf x =
    match x with
    | Inlined ->
      Format.fprintf ppf "inlined"
    | Call (c, _) ->
      Format.fprintf ppf "(%a)" print c
    | Closure (c, _) ->
      Format.fprintf ppf "%a" (print_name ~print_functor:true) c
    | Module (c, _) ->
      Format.fprintf ppf "%s." c
    | Specialised ->
      (* printing nothing because it's already done in the name *)
      Format.fprintf ppf ""
    | SpecialisedCall ->
      Format.fprintf ppf "specialised call"

  and print ppf (f, l) =
    Format.fprintf ppf "%s." f;
    List.iter (fun x ->
      print_atom ppf x;
      match x with
      | Module _ -> ()
      | _ -> Format.fprintf ppf " "
    ) l

  let get_compressed_path (root_f, root) (leaf_f, leaf) =
    let rec aux root leaf =
      match root, leaf with
      | atom :: root, atom' :: leaf when
          compare_atom atom atom' = 0 ->
        let r = aux root leaf in
        if r = [] then atom' :: [] else r
      | _ -> leaf
    in
    if root_f = leaf_f then
      leaf_f, aux root leaf
    else
      leaf_f, leaf

  let strip_call_attributes (path_f, path) =
    path_f,
    List.filter (function
      | Inlined | Specialised | SpecialisedCall -> false
      | _ -> true)
      path

  let file = fst

  let append_atom atom (f, p) =
    f, p @ [atom]
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

let history_to_path ~modname (history : History.t) : Path.t =
  let p =
    history
    |> List.map node_to_atom
    |> List.rev
  in
  (modname, p)


let note_entering_call t ~dbg_name ~dbg
      ~absolute_inlining_history ~cunit_name =
  let absolute_inlining_history =
    (* adding a placeholder call node to represent this call inside the
       absolute history. Its absolute path does not matters as it will
       be stripped during the conversion *)
    History.Call(dbg_name, dbg, Path.empty "") :: absolute_inlining_history
    |> history_to_path ~modname:cunit_name
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

let path_to_definition modpath (path_f, path) =
  let rec convert path file (acc : Definition.t) =
    match (path : Path.path) with
    | Call((def_file, def_path), _) :: _ ->
      convert (List.rev def_path) def_file acc
    | Module(name, _) :: r ->
      convert r file (Definition.Module name :: acc)
    | Closure(n, d) :: r ->
      begin match n with
      | SpecialisedFunction _ ->
        convert r file acc
      | n ->
        convert r file (Definition.Closure(n, d) :: acc)
      end
    | Inlined :: r | Specialised :: r | SpecialisedCall :: r ->
      convert r file acc
    | [] ->
      if file <> modpath then
        Definition.File(file) :: acc
      else
        acc
  in
  convert (List.rev path) path_f Definition.empty

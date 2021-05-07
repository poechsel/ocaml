(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Const = Reg_width_things.Const

type coercion_to_canonical = Coercion.t
type map_to_canonical = coercion_to_canonical Name.Map.t

let compose_map_values_exn map ~then_:coercion =
  if Coercion.is_id coercion then map else
    Name.Map.map (fun old_coercion ->
      Coercion.compose_exn old_coercion ~then_:coercion
    ) map

let fatal_inconsistent ~func_name elt coercion1 coercion2 =
  Misc.fatal_errorf "[%s] maps with inconsistent  element/coercion couples; \
                     %a has coercions %a and %a"
    func_name
    Name.print elt
    Coercion.print coercion1
    Coercion.print coercion2

let map_inter map1 map2 =
  Name.Map.merge (fun elt coercion1 coercion2 ->
    match coercion1, coercion2 with
    | None, None | Some _, None | None, Some _ -> None
    | Some coercion1, Some coercion2 ->
      if Coercion.equal coercion1 coercion2 then
        Some coercion1
      else
        fatal_inconsistent ~func_name:"Aliases.map_inter" elt coercion1 coercion2)
    map1
    map2

let map_union map1 map2 =
  Name.Map.union (fun elt coercion1 coercion2 ->
    match coercion1, coercion2 with
    | coercion1, coercion2 ->
    if Coercion.equal coercion1 coercion2 then
      Some coercion1
    else
      fatal_inconsistent ~func_name:"Aliases.map_union" elt coercion1 coercion2)
    map1
    map2

module Aliases_of_canonical_element : sig
  type t

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : t
  val is_empty : t -> bool

  val add : t -> Name.t -> coercion_to_canonical:Coercion.t -> Name_mode.t -> t

  val find_earliest_candidates
     : t
    -> filter_by_scope:(Name_mode.t -> map_to_canonical -> map_to_canonical)
    -> min_name_mode:Name_mode.t
    -> map_to_canonical option

  val all : t -> map_to_canonical

  val mem : t -> Name.t -> bool

  val union : t -> t -> t
  val inter : t -> t -> t

  val rename : (Name.t -> Name.t) -> t -> t

  val merge : t -> t -> t

  val compose : t -> then_:Coercion.t -> t
end = struct
  type t = {
    aliases : map_to_canonical Name_mode.Map.t;
    all : map_to_canonical;
  }

  let invariant { aliases; all; } =
    (* The elements in [aliases] have disjoint set of keys. *)
    let aliases_union : map_to_canonical =
      Name_mode.Map.fold (fun _name_mode map acc ->
        Name.Map.union (fun elt _coercion1 _coercion2 ->
          Misc.fatal_errorf "[Aliases_of_canonical_element.invariant]: \
                             element %a appears in several modes"
            Name.print elt)
          map
          acc)
        aliases
        Name.Map.empty
    in
    (* [all] is the union of all elements in [aliases] *)
    if Name.Map.equal Coercion.equal all aliases_union then
      ()
    else
      Misc.fatal_errorf "[Aliases_of_canonical_element.invariant]: \
                         [aliases] and [all] are not consistent"

  let print ppf { aliases; all = _; } =
    Name_mode.Map.print (Name.Map.print Coercion.print) ppf aliases

  let empty = {
    aliases = Name_mode.Map.empty;
    all = Name.Map.empty;
  }

  let is_empty t = Name.Map.is_empty t.all

  let add t elt ~coercion_to_canonical name_mode =
    if Name.Map.mem elt t.all then begin
      Misc.fatal_errorf "%a already added to [Aliases_of_canonical_element]: \
                         %a"
        Name.print elt
        print t
    end;
    let aliases =
      Name_mode.Map.update name_mode
        (function
          | None -> Some (Name.Map.singleton elt coercion_to_canonical)
          | Some elts ->
            if !Clflags.flambda_invariant_checks then begin
              assert (not (Name.Map.mem elt elts))
            end;
            Some (Name.Map.add elt coercion_to_canonical elts))
        t.aliases
    in
    let all = Name.Map.add elt coercion_to_canonical t.all in
    { aliases;
      all;
    }

  let find_earliest_candidates t ~filter_by_scope ~min_name_mode =
    Name_mode.Map.fold (fun order aliases res_opt ->
        match res_opt with
        | Some _ -> res_opt
        | None ->
          begin match
            Name_mode.compare_partial_order
              order min_name_mode
          with
          | None -> None
          | Some result ->
            if result >= 0 then
              let aliases = filter_by_scope order aliases in
              if Name.Map.is_empty aliases then None else Some aliases
            else None
          end)
      t.aliases
      None

  let mem t elt =
    Name.Map.mem elt t.all

  let all t = t.all

  let union t1 t2 =
    let aliases : map_to_canonical Name_mode.Map.t=
      Name_mode.Map.union (fun _order elts1 elts2 ->
        Some (map_union elts1 elts2))
        t1.aliases t2.aliases
    in
    let t =
      { aliases;
        all = map_union t1.all t2.all;
      }
    in
    invariant t; (* CR xclerc for xclerc: not guaranteed to hold *)
    t

  let inter t1 t2 =
    let aliases =
      Name_mode.Map.merge (fun _order elts1 elts2 ->
        match elts1, elts2 with
        | None, None | Some _, None | None, Some _ -> None
        | Some elts1, Some elts2 ->
          Some (map_inter elts1 elts2))
        t1.aliases t2.aliases
    in
    let t =
      { aliases;
        all = map_inter t1.all t2.all;
      }
    in
    invariant t;
    t

  let rename rename_name { aliases; all } =
    let map_name elts =
      Name.Map.fold (fun elt coercion acc ->
        Name.Map.add (rename_name elt) coercion acc)
        elts
        Name.Map.empty
    in
    let aliases = Name_mode.Map.map map_name aliases in
    let all = map_name all in
    let t = { aliases; all } in
    invariant t; (* CR xclerc for xclerc: not guaranteed to hold *)
    t

  let merge t1 t2 =
    let aliases =
      Name_mode.Map.union (fun _mode map1 map2 ->
        Some (map_union map1 map2)
      )
        t1.aliases
        t2.aliases
    in
    let all = map_union t1.all t2.all in
    let t = { aliases; all; } in
    invariant t; (* CR xclerc for xclerc: not guaranteed to hold *)
    t

  let compose { aliases; all; } ~then_ =
    let f m =
      Name.Map.map (Coercion.compose_exn ~then_) m
    in
    let aliases = Name_mode.Map.map f aliases in
    let all = f all in
    { aliases; all; }
end

module Alias_set = struct
  type t = {
    const : Const.t option;
    names : map_to_canonical;
  }

  let empty = { const = None; names = Name.Map.empty; }

  let create ~canonical_element ~alias_names 
      ~coercion_from_canonical_to_element =
    Simple.pattern_match canonical_element
      ~const:(fun canonical_const ->
        let const = Some canonical_const in
        let names = alias_names in
        { const; names })
      ~name:(fun canonical_name ->
        let const = None in
        let names = 
          Name.Map.add 
            canonical_name
            coercion_from_canonical_to_element
            alias_names
        in
        { const; names })

  let singleton simple =
    Simple.pattern_match simple
      ~const:(fun const ->
        { const = Some const; names = Name.Map.empty; })
      ~name:(fun name ->
        { const = None; names = Name.Map.singleton name Coercion.id })

  let get_singleton { const; names; } =
    match const with
    | Some const ->
      if Name.Map.is_empty names then Some (Simple.const const) else None
    | None ->
      Name.Map.get_singleton names
      |> Option.map (fun (name, coercion) ->
           Simple.with_coercion (Simple.name name) coercion)

  let print ppf { const; names; } =
    let none ppf () =
      Format.fprintf ppf "@<0>%s()" (Flambda_colours.elide ())
    in
    Format.fprintf ppf
      "@[<hov 1>(\
           @[<hov 1>(const@ %a)@]@ \
           @[<hov 1>(names@ %a)@]\
       @]"
       (Format.pp_print_option Const.print ~none) const
       (Name.Map.print Coercion.print) names

  let inter
        { const = const1; names = names1; }
        { const = const2; names = names2; } =
    let const =
      match const1, const2 with
      | Some const1, Some const2 when Const.equal const1 const2 -> Some const1
      | _, _ -> None
    in
    let names = map_inter names1 names2 in
    { const; names; }

  let filter { const; names; } ~f =
    let const =
      match const with
      | Some const when f (Simple.const const) -> Some const
      | _ -> None
    in
    let names =
      Name.Map.filter (fun name coercion ->
          let simple = Simple.with_coercion (Simple.name name) coercion in
          f simple
        ) names
    in
    { const; names; }

  let find_best { const; names; } =
    match const with
    | Some const -> Some (Simple.const const)
    | None ->
      let key_is_symbol key _data = Name.is_symbol key in
      let (symbols, vars) = Name.Map.partition key_is_symbol names in
      match Name.Map.min_binding_opt symbols with
      | Some (symbol, coercion) ->
        Some (Simple.with_coercion (Simple.name symbol) coercion)
      | None ->
        match Name.Map.min_binding_opt vars with
        | Some (var, coercion) ->
          Some (Simple.with_coercion (Simple.name var) coercion)
        | None ->
          None
end


type t = {
  canonical_elements : (Simple.t * coercion_to_canonical) Name.Map.t;
  (* Canonical elements that have no known aliases are not included in
     [canonical_elements]. *)
  aliases_of_canonical_names : Aliases_of_canonical_element.t Name.Map.t;
  (* For [elt |-> aliases] in [aliases_of_canonical_names], then
     [aliases] never includes [elt]. *)
  (* CR mshinwell: check this always holds *)
  aliases_of_consts : Aliases_of_canonical_element.t Const.Map.t;
  binding_times_and_modes : Binding_time.With_name_mode.t Name.Map.t;
  (* Binding times and name modes define an order on the elements.
     The canonical element for a set of aliases is always the minimal
     element for this order, which is different from the order used
     for creating sets and maps. *)
}

(* Canonical elements can be seen as a collection of star graphs:

   canon_i <--[coercion_i_0]-- elem_i_0
       ^ ^--[...]-- ...
        \--[coercion_i_m]-- elem_i_m

   ...

   canon_j <--[coercion_j_0]-- elem_j_0
       ^ ^--[...]-- ...
        \--[coercion_j_n]-- elem_j_n


   stored as a map:

   canonical_elements[elem_i_0] = (canon_i, coercion_i_0)
   ...
   canonical_elements[elem_i_m] = (canon_i, coercion_i_m)

   ...

   canonical_elements[elem_j_0] = (canon_j, coercion_j_0)
   ...
   canonical_elements[elem_j_n] = (canon_j, coercion_j_n)
*)

let print ppf { canonical_elements; aliases_of_canonical_names;
                aliases_of_consts; binding_times_and_modes; } =
  let print_element_and_coercion ppf (elt, coercion) =
    Format.fprintf ppf "@[<hov 1>(\
                        %a@ \
                        @[<hov 1>@<0>%s(coercion@ %a)@<0>%s@]\
                        )@]"
      Simple.print elt
      (if Coercion.is_id coercion
      then Flambda_colours.elide ()
      else Flambda_colours.normal ())
      Coercion.print coercion
      (Flambda_colours.normal ())
  in
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(canonical_elements@ %a)@]@ \
      @[<hov 1>(aliases_of_canonical_names@ %a)@]@ \
      @[<hov 1>(aliases_of_consts@ %a)@]@ \
      @[<hov 1>(binding_times_and_modes@ %a)@]\
      )@]"
    (Name.Map.print print_element_and_coercion) canonical_elements
    (Name.Map.print Aliases_of_canonical_element.print)
    aliases_of_canonical_names
    (Const.Map.print Aliases_of_canonical_element.print)
    aliases_of_consts
    (Name.Map.print Binding_time.With_name_mode.print)
    binding_times_and_modes

let name_defined_earlier t alias ~than =
  let info1 = Name.Map.find alias t.binding_times_and_modes in
  let info2 = Name.Map.find than t.binding_times_and_modes in
  Binding_time.strictly_earlier
    (Binding_time.With_name_mode.binding_time info1)
    ~than:(Binding_time.With_name_mode.binding_time info2)

let defined_earlier t alias ~than =
  Simple.pattern_match than
    ~const:(fun _ -> false)
    ~name:(fun than ->
      Simple.pattern_match alias
        ~const:(fun _ -> true)
        ~name:(fun alias -> name_defined_earlier t alias ~than))

let binding_time_and_name_mode t elt =
  Simple.pattern_match elt
    ~const:(fun _ ->
      Binding_time.With_name_mode.create
        Binding_time.consts_and_discriminants
        Name_mode.normal)
    ~name:(fun elt -> Name.Map.find elt t.binding_times_and_modes)

let name_mode_unscoped t elt =
  Binding_time.With_name_mode.name_mode (binding_time_and_name_mode t elt)

let name_mode t elt ~min_binding_time =
  Binding_time.With_name_mode.scoped_name_mode
    (binding_time_and_name_mode t elt)
    ~min_binding_time

let invariant t =
  if !Clflags.flambda_invariant_checks then begin
    let all_aliases_of_names : map_to_canonical =
      Name.Map.fold (fun canonical_element aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases;
          let aliases = Aliases_of_canonical_element.all aliases in
          if not (Name.Map.for_all (fun elt _coercion ->
            name_defined_earlier t canonical_element ~than:elt) aliases)
          then begin
            Misc.fatal_errorf "Canonical element %a is not earlier than \
                all of its aliases:@ %a"
              Name.print canonical_element
              print t
          end;
          if Name.Map.mem canonical_element aliases then begin
            Misc.fatal_errorf "Canonical element %a occurs in alias set:@ %a"
              Name.print canonical_element
              (Name.Map.print Coercion.print) aliases
          end;
          if Name.Map.inter_domain_is_non_empty aliases all_aliases then
          begin
            Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
          end;
          map_union aliases all_aliases)
        t.aliases_of_canonical_names
        Name.Map.empty
    in
    let _all_aliases : map_to_canonical =
      Const.Map.fold (fun _const aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases;
          let aliases = Aliases_of_canonical_element.all aliases in
          if Name.Map.inter_domain_is_non_empty aliases all_aliases then
          begin
            Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
          end;
          Name.Map.disjoint_union aliases all_aliases)
        t.aliases_of_consts
        all_aliases_of_names
    in
    ()
  end

let empty = {
  (* CR mshinwell: Rename canonical_elements, maybe to
     aliases_to_canonical_elements. *)
  canonical_elements = Name.Map.empty;
  aliases_of_canonical_names = Name.Map.empty;
  aliases_of_consts = Const.Map.empty;
  binding_times_and_modes = Name.Map.empty;
}

type canonical =
  | Is_canonical
  | Alias_of_canonical of {
      canonical_element : Simple.t;
      coercion_to_canonical : coercion_to_canonical;
    }

let canonical t element : canonical =
  Simple.pattern_match element
    ~const:(fun _ -> Is_canonical)
    ~name:(fun name ->
      match Name.Map.find name t.canonical_elements with
      | exception Not_found -> Is_canonical
      | canonical_element, coercion_to_canonical ->
        if !Clflags.flambda_invariant_checks then begin
          assert (not (Simple.equal element canonical_element))
        end;
        Alias_of_canonical { canonical_element; coercion_to_canonical; })

let get_aliases_of_canonical_element t ~canonical_element =
  let name name =
    Name.Map.find name t.aliases_of_canonical_names
  in
  let const const =
    Const.Map.find const t.aliases_of_consts
  in
  match Simple.pattern_match canonical_element ~name ~const with
  | exception Not_found -> Aliases_of_canonical_element.empty
  | aliases -> aliases

(*
   before
   ~~~~~~
   canonical_element <--[coercion_ce_0]-- ce_0
     ^ ^--[...]-- ...
      \--[coercion_ce_m]-- ce_m
   to_be_demoted <--[coercion_tbd_0]-- tbd_0
     ^ ^--[...]-- ...
      \--[coercion_tbd_n]-- tbd_n

   i.e.

   canonical_elements[ce_0] = (canonical_element, coercion_ce_0)
   ...
   canonical_elements[ce_m] = (canonical_element, coercion_ce_m)
   canonical_elements[tbd_0] = (to_be_demoted, coercion_tbd_0)
   ...
   canonical_elements[tbd_n] = (to_be_demoted, coercion_tbd_n)


   after
   ~~~~~
   canonical_element <--[coercion_ce_0]-- ce_0
     ^ ^ ^ ^ ^ ^--[...]-- ...
     | | | |  \--[coercion_ce_m]-- ce_m
     | | | \--[coercion_to_canonical]-- to_be_demoted
     | | \--[compose(coercion_tbd_0, coercion_to_canonical)]-- tbd_0
     | \--[...]-- ...
     \--[compose(coercion_tbd_n, coercion_to_canonical)]-- tbd_n

   i.e.

   canonical_elements[ce_0] = (canonical_element, coercion_ce_0)
   ...
   canonical_elements[ce_m] = (canonical_element, coercion_ce_m)
   canonical_elements[to_be_demoted] = (canonical_element, coercion_to_canonical)
   canonical_elements[tbd_0] = (canonical_element, compose(coercion_tbd_0, coercion_to_canonical))
   ...
   canonical_elements[tbd_n] = (canonical_element, compose(coercion_tbd_n, coercion_to_canonical))

*)
let add_alias_between_canonical_elements t ~canonical_element
      ~coercion_to_canonical:coercion_to_canonical ~to_be_demoted =
  if Simple.equal canonical_element to_be_demoted then begin
    if Coercion.is_id coercion_to_canonical then begin
      t
    end else
      Misc.fatal_errorf
        "Cannot add an alias of %a@ to itself with a non-identity coercion@ %a"
        Simple.print canonical_element
        Coercion.print coercion_to_canonical
  end else
    let name_to_be_demoted =
      Simple.pattern_match to_be_demoted
        ~const:(fun c ->
          Misc.fatal_errorf
            "Can't demote const %a@ (while adding alias to@ %a)"
          Const.print c
          Simple.print canonical_element)
        ~name:(fun name -> name)
    in
    let aliases_of_to_be_demoted =
      get_aliases_of_canonical_element t ~canonical_element:to_be_demoted
    in
    if !Clflags.flambda_invariant_checks then begin
      Simple.pattern_match canonical_element
        ~const:(fun _ -> ())
        ~name:(fun canonical_element ->
          assert (not (Aliases_of_canonical_element.mem
            aliases_of_to_be_demoted canonical_element)))
    end;
    let canonical_elements =
      t.canonical_elements
      |> Name.Map.fold (fun alias coercion_to_to_be_demoted canonical_elements ->
        let coercion_to_canonical =
          Coercion.compose_exn coercion_to_to_be_demoted ~then_:coercion_to_canonical
        in
        Name.Map.add alias (canonical_element, coercion_to_canonical) canonical_elements)
        (Aliases_of_canonical_element.all aliases_of_to_be_demoted)
      |> Name.Map.add name_to_be_demoted (canonical_element, coercion_to_canonical)
    in
    let aliases_of_canonical_element =
      get_aliases_of_canonical_element t ~canonical_element
    in
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Aliases_of_canonical_element.mem
        aliases_of_canonical_element name_to_be_demoted));
      assert (Aliases_of_canonical_element.is_empty (
        Aliases_of_canonical_element.inter
          aliases_of_canonical_element aliases_of_to_be_demoted))
    end;
    let aliases =
      Aliases_of_canonical_element.add
        (Aliases_of_canonical_element.union
           (Aliases_of_canonical_element.compose aliases_of_to_be_demoted ~then_:coercion_to_canonical)
           aliases_of_canonical_element)
        name_to_be_demoted
        ~coercion_to_canonical
        (name_mode_unscoped t to_be_demoted)
    in
    let aliases_of_canonical_names =
      Name.Map.remove name_to_be_demoted t.aliases_of_canonical_names
    in
    let aliases_of_canonical_names, aliases_of_consts =
      Simple.pattern_match canonical_element
        ~name:(fun name ->
          Name.Map.add (* replace *) name aliases aliases_of_canonical_names,
          t.aliases_of_consts)
        ~const:(fun const ->
          aliases_of_canonical_names,
          Const.Map.add (* replace *) const aliases t.aliases_of_consts)
    in
    let res =
    { canonical_elements;
      aliases_of_canonical_names;
      aliases_of_consts;
      binding_times_and_modes = t.binding_times_and_modes;
    } in
    invariant res;
    res

type to_be_demoted = Demote_canonical_element1 | Demote_canonical_element2

let choose_canonical_element_to_be_demoted t ~canonical_element1
      ~canonical_element2 =
  if defined_earlier t canonical_element1 ~than:canonical_element2
  then Demote_canonical_element2 else Demote_canonical_element1

(* CR mshinwell: add submodule *)
type add_result = {
  t : t;
  canonical_element : Simple.t;
  alias_of_demoted_element : Simple.t;
}

let invariant_add_result ~original_t { canonical_element; alias_of_demoted_element; t; } =
  if !Clflags.flambda_invariant_checks then begin
    invariant t;
    if not (defined_earlier t canonical_element ~than:alias_of_demoted_element) then begin
      Misc.fatal_errorf "Canonical element %a should be defined earlier \
          than %a after alias addition.@ Original alias tracker:@ %a@ \
          Resulting alias tracker:@ %a"
        Simple.print canonical_element
        Simple.print alias_of_demoted_element
        print original_t
        print t
    end;
    match canonical t alias_of_demoted_element with
    | Is_canonical ->
        Misc.fatal_errorf "Alias %a must not be must not be canonical \
            anymore.@ \
            Original alias tracker:@ %a@ \
            Resulting alias tracker:@ %a"
          Simple.print alias_of_demoted_element
          print original_t
          print t
    | Alias_of_canonical _ -> ()
  end

let add_alias t ~element1 ~coercion_from_element2_to_element1 ~element2 =
  let add ~canonical_element1 ~canonical_element2
        ~coercion_from_element1_to_canonical_element1
        ~coercion_from_element2_to_canonical_element2
        ~coercion_from_canonical_element2_to_canonical_element1 =
    if Simple.equal canonical_element1 canonical_element2
    then
      let canonical_element = canonical_element1 in
      (* We don't have to change anything: since [element1] and [element2] have
         the same canonical element, they must already be aliases. But what to
         return? According to the contract for [add],
         [alias_of_demoted_element] must not be canonical and must equal either
         [element1] or [element2]. Thus we must choose whichever of [element1]
         and [element2] is not canonical. (They cannot both be canonical: if
         [element1] is canonical then it's equal to [canonical_element], and
         the same goes for [element2], but they can't both be equal to
         [canonical_element] since we assume in [add] that they're different. *)
      (* CR lmaurer: These elaborate postconditions are there to avoid breaking
         [Typing_env.add_equations]. It would be better to decouple these
         functions. Per discussions with poechsel and vlaviron, the
         information that [Typing_env] is after (besides the updated [Aliases.t])
         is really:

         1. What aliases need to be updated?
         2. What do those aliases point to now?

         Currently #1 is always exactly one element, but it could be zero in
         this case since nothing needs to be updated.

         So the new [add_result] could be something like:
         {[
           type add_result = {
             t : t;
             updated_aliases : Name.t list;
             new_canonical_element : Simple.t;
           }
         ]}

         (It's not actually necessary that [new_canonical_element] is canonical
         as far as [Typing_env] is concerned, but I think this is easier
         to explain than "representative_of_new_alias_class" or some such.) *)
      let alias_of_demoted_element =
        if Simple.equal element1 canonical_element then element2 else element1
      in
      { t; canonical_element; alias_of_demoted_element; }
    else
      let canonical_element, demoted_canonical, alias_of_demoted_element,
          coercion_from_demoted_canonical_to_canonical,
          coercion_from_demoted_alias_to_demoted_canonical = 
        let which_element =
          choose_canonical_element_to_be_demoted t
            ~canonical_element1 ~canonical_element2
        in
        match which_element with
        | Demote_canonical_element1 ->
          let coercion_from_canonical_element1_to_canonical_element2 =
            Coercion.inverse
              coercion_from_canonical_element2_to_canonical_element1
          in
          canonical_element2, canonical_element1, element1,
          coercion_from_canonical_element1_to_canonical_element2,
          coercion_from_element1_to_canonical_element1
        | Demote_canonical_element2 ->
          canonical_element1, canonical_element2, element2,
          coercion_from_canonical_element2_to_canonical_element1,
          coercion_from_element2_to_canonical_element2
      in
      let t = 
        add_alias_between_canonical_elements
          t
          ~canonical_element
          ~coercion_to_canonical:coercion_from_demoted_canonical_to_canonical
          ~to_be_demoted:demoted_canonical
      in
      let coercion_from_demoted_alias_to_canonical =
        Coercion.compose_exn
          coercion_from_demoted_alias_to_demoted_canonical
          ~then_:coercion_from_demoted_canonical_to_canonical
      in
      let alias_of_demoted_element =
        Simple.with_coercion alias_of_demoted_element
          coercion_from_demoted_alias_to_canonical
      in
      { t;
        canonical_element;
        alias_of_demoted_element;
      }
  in
  match canonical t element1, canonical t element2 with
  | Is_canonical, Is_canonical ->
    let canonical_element1 = element1 in
    let canonical_element2 = element2 in
    let coercion_from_element1_to_canonical_element1 = Coercion.id in
    let coercion_from_element2_to_canonical_element2 = Coercion.id in
    let coercion_from_canonical_element2_to_canonical_element1 =
      coercion_from_element2_to_element1
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1
  | Alias_of_canonical {
      canonical_element = canonical_element1;
      coercion_to_canonical = coercion_from_element1_to_canonical_element1;
    },
    Is_canonical ->
    let canonical_element2 = element2 in
    let coercion_from_element2_to_canonical_element2 = Coercion.id in
    (* element1 <--[c]-- canonical_element2=element2
       +
       canonical_element1 <--[c1] element1
       ~>
       canonical_element1 <--[c1 << c]-- canonical_element2 *)
    let coercion_from_canonical_element2_to_canonical_element1 =
      Coercion.compose_exn coercion_from_element2_to_element1
        ~then_:coercion_from_element1_to_canonical_element1
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1
  | Is_canonical,
    Alias_of_canonical {
      canonical_element = canonical_element2;
      coercion_to_canonical = coercion_from_element2_to_canonical_element2;
    } ->
    let canonical_element1 = element1 in
    let coercion_from_element1_to_canonical_element1 = Coercion.id in
    let coercion_from_canonical_element2_to_canonical_element1 =
      (* canonical_element1=element1 <--[c]-- element2 
         +
         canonical_element2 <--[c2]-- element2
         ~>
         element2 <--[c2^-1]-- canonical_element2
         ~>
         canonical_element1 <--[c << c2^-1]-- canonical_element2
      *)
      Coercion.compose_exn
        (Coercion.inverse coercion_from_element2_to_canonical_element2)
        ~then_:coercion_from_element2_to_element1
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1
  | Alias_of_canonical {
      canonical_element = canonical_element1;
      coercion_to_canonical = coercion_from_element1_to_canonical_element1;
    },
    Alias_of_canonical {
      canonical_element = canonical_element2;
      coercion_to_canonical = coercion_from_element2_to_canonical_element2;
    } ->
    let coercion_from_canonical_element2_to_canonical_element1 =
      (* canonical_element1 <--[c1]-- element1
         canonical_element2 <--[c2]-- element2
         +
         element1 <--[c]-- element2
         ~>
         element2 <--[c2^-1]-- canonical_element2
         ~>
         canonical_element1 <--[c1 << c << c2^-1]-- canonical_element2
         *)
      Coercion.compose_exn
        (Coercion.inverse coercion_from_element2_to_canonical_element2)
        ~then_:(Coercion.compose_exn
          coercion_from_element2_to_element1
          ~then_:coercion_from_element1_to_canonical_element1)
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1

let add t ~element1:element1_with_coercion ~binding_time_and_mode1
      ~element2:element2_with_coercion ~binding_time_and_mode2 =
  let original_t = t in
  (* element1_with_coercion <--[c1]-- element1
     +
     element2_with_coercion <--[c2]-- element2
     ~
     element1 <--[c1^-1]-- element1_with_coercion
     ~
     element1 <--[c1^-1 << c2]-- element2
   *)
  let element1 = element1_with_coercion |> Simple.without_coercion in
  let element2 = element2_with_coercion |> Simple.without_coercion in
  let coercion_from_element2_to_element1 =
    Coercion.compose_exn (Simple.coercion element2_with_coercion)
      ~then_:(Coercion.inverse (Simple.coercion element1_with_coercion))
  in
  if !Clflags.flambda_invariant_checks then begin
    if Simple.equal element1 element2 then begin
      Misc.fatal_errorf
        "Cannot alias an element to itself: %a" Simple.print element1
    end;
    Simple.pattern_match element1
      ~name:(fun _ -> ())
      ~const:(fun const1 ->
        Simple.pattern_match element2
          ~name:(fun _ -> ())
          ~const:(fun const2 ->
            Misc.fatal_errorf
              "Cannot add alias between two consts: %a, %a"
                Const.print const1
                Const.print const2
          ));
  end;
  let add_if_name simple data map =
    Simple.pattern_match simple
      ~const:(fun _ -> map)
      ~name:(fun name -> Name.Map.add name data map)
  in
  let t =
    { t with binding_times_and_modes =
               add_if_name element1 binding_time_and_mode1
                 (add_if_name element2 binding_time_and_mode2
                    t.binding_times_and_modes);
    }
  in
  let add_result = add_alias t ~element1 ~coercion_from_element2_to_element1 ~element2 in
  if !Clflags.flambda_invariant_checks then begin
    invariant_add_result ~original_t add_result
  end;
  add_result

let mem t element =
  Simple.pattern_match element
    ~const:(fun const ->
      Const.Map.mem const t.aliases_of_consts)
    ~name:(fun name ->
      Name.Map.mem name t.binding_times_and_modes)

  (* CR mshinwell: This needs documenting.  For the moment we allow
     relations between canonical elements that are actually incomparable
     under the name mode ordering, and check in [get_canonical_element_exn]
     accordingly.  However maybe we should never allow these situations to
     arise. *)
  (*
  let canonical_mode =
    name_mode t add_result.canonical_element
  in
  let alias_of_mode = name_mode t add_result.alias_of in
  match
    Name_mode.compare_partial_order
      canonical_mode alias_of_mode
  with
  | Some _ -> add_result
  | None ->
    Misc.fatal_errorf "Canonical %a has mode incomparable with %a in:@ %a"
      Simple.print add_result.canonical_element
      Simple.print add_result.alias_of
      print t
  *)

let get_canonical_element_exn t element elt_name_mode ~min_name_mode
      ~min_binding_time =
  let canonical_element, name_mode, coercion_from_canonical_to_element =
    match canonical t element with
    | Is_canonical ->
      element, elt_name_mode, Coercion.id
    | Alias_of_canonical { canonical_element; coercion_to_canonical; } ->
      let name_mode = name_mode t canonical_element ~min_binding_time in
      canonical_element, name_mode, Coercion.inverse coercion_to_canonical
  in
  (*
Format.eprintf "looking for canonical for %a, candidate canonical %a, min order %a\n%!"
  Simple.print element
  Simple.print canonical_element
  Name_mode.print min_name_mode;
*)
  let find_earliest () =
    (* There used to be a shortcut that avoided a consulting the aliases in the
       common case that [element] is itself canonical and has no aliases, since
       then it does not appear in [canonical_elements]. However, this shortcut
       was broken: a canonical element *with* known aliases may still not appea

       in [canonical_elements]. See tests/flambda2-aliases for a test that gave
       incorrect output (saying x/39 had no aliases). It may be worth restoring
       the shortcut, perhaps by returning more information from [canonical]. *)
    let aliases = get_aliases_of_canonical_element t ~canonical_element in
    let filter_by_scope name_mode names =
      if Name_mode.equal name_mode Name_mode.in_types then names
      else
        Name.Map.filter (fun name _coercion ->
            let binding_time_and_mode =
              Name.Map.find name t.binding_times_and_modes
            in
            let scoped_name_mode =
              Binding_time.With_name_mode.scoped_name_mode
                binding_time_and_mode
                ~min_binding_time
            in
            Name_mode.equal name_mode scoped_name_mode)
          names
    in
    match
      Aliases_of_canonical_element.find_earliest_candidates aliases
        ~filter_by_scope ~min_name_mode
    with
    | Some at_earliest_mode ->
      (* Aliases_of_canonical_element.find_earliest_candidates only returns
         non-empty sets *)
      assert (not (Name.Map.is_empty at_earliest_mode));
      let earliest, coercion_from_earliest_to_canonical =
        Name.Map.fold (fun elt coercion ((min_elt, _min_coercion) as min_binding) ->
            if name_defined_earlier t elt ~than:min_elt
            then elt, coercion
            else min_binding)
          at_earliest_mode
          (Name.Map.min_binding at_earliest_mode)
      in
      let coercion_from_earliest_to_element =
        Coercion.compose_exn coercion_from_earliest_to_canonical
          ~then_:coercion_from_canonical_to_element
      in
      Simple.with_coercion (Simple.name earliest) coercion_from_earliest_to_element
    | None -> raise Not_found
  in
  match
    Name_mode.compare_partial_order name_mode min_name_mode
  with
  | None -> find_earliest ()
  | Some c ->
    if c >= 0 then
      Simple.with_coercion canonical_element coercion_from_canonical_to_element
    else find_earliest ()

let get_aliases t element =
  match canonical t element with
  | Is_canonical ->
    let canonical_element = element in
    let names_with_coercions_to_canonical =
      Aliases_of_canonical_element.all
        (get_aliases_of_canonical_element t ~canonical_element)
    in
    let coercion_from_canonical_to_element = Coercion.id in
    Alias_set.create
      ~canonical_element
      ~coercion_from_canonical_to_element
      ~alias_names:names_with_coercions_to_canonical
  | Alias_of_canonical { canonical_element;
      coercion_to_canonical = coercion_from_element_to_canonical; } ->
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Simple.equal element canonical_element))
    end;
    let names_with_coercions_to_canonical =
      Aliases_of_canonical_element.all
        (get_aliases_of_canonical_element t ~canonical_element)
    in
    let coercion_from_canonical_to_element =
      Coercion.inverse coercion_from_element_to_canonical
    in
    (* If any composition fails, then our coercions are inconsistent somehow,
       which should only happen when meeting *)
    let names_with_coercions_to_element =
      compose_map_values_exn names_with_coercions_to_canonical
        ~then_:coercion_from_canonical_to_element
    in
    
    if !Clflags.flambda_invariant_checks then begin
      let element_coerced_to_canonical =
        Simple.apply_coercion_exn element coercion_from_element_to_canonical
      in
      (* These aliases are all equivalent to the canonical element, and so is
         our original [element] if we coerce it first, so the coerced form of
         [element] should be among the aliases. *)
      assert (Name.Map.exists 
        (fun name coercion_from_name_to_canonical ->
          let name_coerced_to_canonical =
            Simple.apply_coercion_exn
              (Simple.name name)
              coercion_from_name_to_canonical
          in
          Simple.equal element_coerced_to_canonical name_coerced_to_canonical
        ) names_with_coercions_to_canonical)
    end;
    Alias_set.create
      ~canonical_element
      ~coercion_from_canonical_to_element
      ~alias_names:names_with_coercions_to_element

let all_ids_for_export { canonical_elements = _;
                         aliases_of_canonical_names = _;
                         aliases_of_consts = _;
                         binding_times_and_modes; } =
  Name.Map.fold (fun elt _binding_time_and_mode ids ->
    Ids_for_export.add_name ids elt)
    binding_times_and_modes
    Ids_for_export.empty

let apply_renaming
      { canonical_elements;
        aliases_of_canonical_names;
        aliases_of_consts;
        binding_times_and_modes; }
      renaming =
  let rename_name = Renaming.apply_name renaming in
  let rename_simple = Renaming.apply_simple renaming in
  let canonical_elements =
    Name.Map.fold (fun elt (canonical, coercion) acc ->
      Name.Map.add (rename_name elt) (rename_simple canonical, coercion) acc)
      canonical_elements
      Name.Map.empty
  in
  let aliases_of_canonical_names =
    Name.Map.fold (fun canonical aliases acc ->
        Name.Map.add (rename_name canonical)
          (Aliases_of_canonical_element.rename rename_name aliases)
          acc)
      aliases_of_canonical_names
      Name.Map.empty
  in
  let aliases_of_consts =
    Const.Map.map (Aliases_of_canonical_element.rename rename_name)
      aliases_of_consts
  in
  let binding_times_and_modes =
    Name.Map.fold (fun name binding_time_and_mode acc ->
        Name.Map.add (rename_name name) binding_time_and_mode acc)
      binding_times_and_modes
      Name.Map.empty
  in
  { canonical_elements;
    aliases_of_canonical_names;
    aliases_of_consts;
    binding_times_and_modes;
  }

let merge t1 t2 =
  let canonical_elements =
    Name.Map.disjoint_union
      t1.canonical_elements
      t2.canonical_elements
  in
  (* Warning: we assume that the aliases in the two alias trackers are disjoint,
     but nothing stops them from sharing a canonical element. For instance, if
     multiple compilation units define aliases to the same canonical symbol,
     that symbol will be a canonical element in both of the units' alias
     trackers, and thus their [aliases_of_canonical_names] will have a key in
     common. *)
  let merge_aliases _canonical aliases1 aliases2 =
    Some (Aliases_of_canonical_element.merge aliases1 aliases2)
  in
  let aliases_of_canonical_names =
    Name.Map.union merge_aliases
      t1.aliases_of_canonical_names
      t2.aliases_of_canonical_names
  in
  let aliases_of_consts =
    Const.Map.union merge_aliases
      t1.aliases_of_consts
      t2.aliases_of_consts
  in

  let symbol_data =
    Binding_time.With_name_mode.create
      Binding_time.symbols
      Name_mode.normal
  in
  let binding_times_and_modes =
    Name.Map.union (fun name data1 data2 ->
        Name.pattern_match name
          ~var:(fun var ->
            (* TODO: filter variables on export and restore fatal_error *)
            if Binding_time.(equal (With_name_mode.binding_time data1)
                               imported_variables)
            then Some data2
            else if Binding_time.(equal (With_name_mode.binding_time data2)
                               imported_variables)
            then Some data1
            else
              Misc.fatal_errorf
                "Variable %a is present in multiple environments"
                Variable.print var)
          ~symbol:(fun _sym ->
            assert (Binding_time.With_name_mode.equal data1 symbol_data);
            assert (Binding_time.With_name_mode.equal data2 symbol_data);
            Some data1))
      t1.binding_times_and_modes
      t2.binding_times_and_modes
  in
  { canonical_elements;
    aliases_of_canonical_names;
    aliases_of_consts;
    binding_times_and_modes;
  }

let get_canonical_ignoring_name_mode t name =
  let elt = Simple.name name in
  match canonical t elt with
  | Is_canonical ->
    elt
  | Alias_of_canonical { canonical_element; coercion_to_canonical } ->
    let coercion_from_canonical = Coercion.inverse coercion_to_canonical in
    Simple.apply_coercion_exn canonical_element coercion_from_canonical

let clean_for_export
      { canonical_elements;
        aliases_of_canonical_names;
        aliases_of_consts;
        binding_times_and_modes; } =
  (* CR vlaviron: This function is kept as a reminder that we'd like
     to remove unreachable entries at some point. *)
  { canonical_elements;
    aliases_of_canonical_names;
    aliases_of_consts;
    binding_times_and_modes;
  }
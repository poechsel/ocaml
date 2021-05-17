(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Depth_variable

let free_names t = Name_occurrences.singleton_depth_variable t

let apply_renaming t perm = Renaming.apply_depth_variable perm t

let all_ids_for_export _ =
  (* Depth variables don't use the integer id system *)
  Ids_for_export.empty

let add_to_name_permutation t ~guaranteed_fresh perm =
  Renaming.add_fresh_depth_variable perm t ~guaranteed_fresh

let name_permutation t ~guaranteed_fresh =
  add_to_name_permutation t ~guaranteed_fresh Renaming.empty

let singleton_occurrence_in_terms t =
  Name_occurrences.singleton_depth_variable t

let add_occurrence_in_terms t occs =
  Name_occurrences.add_depth_variable occs t

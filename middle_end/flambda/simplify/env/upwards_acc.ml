(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module DA = Downwards_acc
module LCS = Lifted_constant_state
module TE = Flambda_type.Typing_env
module UE = Simplify_envs.Upwards_env

module Static_const = Flambda.Static_const

type t = {
  uenv : UE.t;
  creation_dacc : DA.t;
  code_age_relation : Code_age_relation.t;
  lifted_constants : LCS.t;
  all_code : Exported_code.t;
  name_occurrences : Name_occurrences.t;
  used_closure_vars : Name_occurrences.t;
  shareable_constants : Symbol.t Static_const.Map.t;
}

let print ppf
      { uenv; creation_dacc = _; code_age_relation; lifted_constants;
        name_occurrences; used_closure_vars; all_code = _;
        shareable_constants; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(uenv@ %a)@]@ \
      @[<hov 1>(code_age_relation@ %a)@]@ \
      @[<hov 1>(lifted_constants@ %a)@]@ \
      @[<hov 1>(name_occurrences@ %a)@]@ \
      @[<hov 1>(used_closure_vars@ %a)@]@ \
      @[<hov 1>(shareable_constants@ %a)@]\
      )@]"
    UE.print uenv
    Code_age_relation.print code_age_relation
    LCS.print lifted_constants
    Name_occurrences.print name_occurrences
    Name_occurrences.print used_closure_vars
    (Static_const.Map.print Symbol.print) shareable_constants

let create uenv dacc =
  { uenv;
    creation_dacc = dacc;
    code_age_relation = TE.code_age_relation (DA.typing_env dacc);
    lifted_constants = LCS.empty;
    all_code = Exported_code.empty;
    name_occurrences = Name_occurrences.empty;
    (* [used_closure_vars] must be kept separate from the normal free
       names tracking in [name_occurrences], since it is always accumulated,
       and never saved and restored (like free name information is when
       dealing with a [Let_cont]). *)
    used_closure_vars = DA.used_closure_vars dacc;
    shareable_constants = DA.shareable_constants dacc;
  }

let creation_dacc t = t.creation_dacc
let uenv t = t.uenv
let code_age_relation t = t.code_age_relation
let lifted_constants t = t.lifted_constants

(* Don't add empty LCS to the list *)

let add_outermost_lifted_constant t const =
  { t with
    lifted_constants = LCS.add_outermost t.lifted_constants const;
  }

let with_lifted_constants t lifted_constants =
  { t with
    lifted_constants;
  }

let no_lifted_constants t = LCS.is_empty t.lifted_constants

let map_uenv t ~f =
  { t with
    uenv = f t.uenv;
  }

let with_uenv t uenv =
  { t with
    uenv;
  }

let remember_code_for_cmx t code =
  let all_code = Exported_code.add_code code t.all_code in
  { t with all_code; }

let all_code t = t.all_code

let name_occurrences t = t.name_occurrences

let with_name_occurrences t ~name_occurrences =
  if name_occurrences == t.name_occurrences then t
  else { t with name_occurrences; }

let clear_name_occurrences t =
  with_name_occurrences t ~name_occurrences:Name_occurrences.empty

let add_free_names t free_names =
  let name_occurrences =
    Name_occurrences.union t.name_occurrences free_names
  in
  { t with name_occurrences; }

let used_closure_vars t = t.used_closure_vars

let shareable_constants t = t.shareable_constants

let remove_all_occurrences_of_free_names t to_remove =
  let name_occurrences =
    Name_occurrences.diff t.name_occurrences to_remove
  in
  { t with name_occurrences; }

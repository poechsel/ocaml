(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = {
  compilation_unit : Compilation_unit.t;
  previous_compilation_units : Compilation_unit.t list;
  name : string;
  name_stamp : int;
}

include Identifiable.Make(struct
  type nonrec t = t

  let compare { name_stamp = ns1; compilation_unit = cu1;
                previous_compilation_units = pcus1; name = _ }
              { name_stamp = ns2; compilation_unit = cu2;
                previous_compilation_units = pcus2; name = _ } =
    match Int.compare ns1 ns2 with
    | 0 ->
      begin match Compilation_unit.compare cu1 cu2 with
      | 0 ->
        Misc.Stdlib.List.compare Compilation_unit.compare pcus1 pcus2
      | c -> c
      end
    | c -> c

  let hash { compilation_unit; previous_compilation_units; name_stamp;
             name = _ } =
    Hashtbl.hash (Compilation_unit.hash compilation_unit,
                  List.map Compilation_unit.hash previous_compilation_units,
                  name_stamp)

  let equal t1 t2 = compare t1 t2 = 0

  let print ppf t =
    Format.fprintf ppf "@<0>%s" (Flambda_colours.depth_variable ());
    Format.fprintf ppf "%s/%d" t.name t.name_stamp;
    Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

type exported = t

let previous_name_stamp = ref (-1)

let create name : t =
  let name_stamp =
    incr previous_name_stamp;
    !previous_name_stamp
  in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let previous_compilation_units = [] in
  { compilation_unit; previous_compilation_units; name; name_stamp; }

let rename { name; name_stamp = _; compilation_unit = _;
             previous_compilation_units = _; } =
  create name

let name t = t.name

let name_stamp t = t.name_stamp

let print_with_cache ~cache:_ ppf t = print ppf t

let export t = t

let import data = data

let map_compilation_unit f data =
  let new_compilation_unit = f data.compilation_unit in
  if Compilation_unit.equal new_compilation_unit data.compilation_unit
  then data
  else
    { data with compilation_unit = new_compilation_unit;
                previous_compilation_units =
                  data.compilation_unit :: data.previous_compilation_units;
    }

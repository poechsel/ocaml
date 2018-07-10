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


module IH = Inlining_history

let log = Hashtbl.create 5
let external_logs = Hashtbl.create 5


let record_decision decision ~round ~closure_stack =
  if !Clflags.inlining_report then begin
    match (closure_stack : IH.History.t) with
    | []
    | Module _ :: _
    | Inlined :: _
    | SpecialisedCall :: _ ->
      Misc.fatal_errorf "record_decision: missing Call node"
    | Specialised :: _
    | Closure _ :: _
    | Call _ :: _ ->
      Hashtbl.replace log closure_stack (round, decision)
  end

module Inlining_report = struct
  module Place_map = Map.Make(struct
      type t = IH.Path.atom
      let compare = IH.Path.compare_atom
    end)

  type decision =
    | Decision of int * Inlining_stats_types.Decision.t
    | Reference of IH.Path.t

  type t = node Place_map.t

  and node =
    | Closure of t
    | Module of t
    | Call of call

  and call =
    { decision: decision;
      inlined: t option;
      specialised: t option;
      specialised_call: t option;
    }

  let empty_call path =
    { decision = Reference path;
      inlined = None;
      specialised = None;
      specialised_call = None;
    }


  (* Prevented or unchanged decisions may be overridden by a later look at the
     same call. Other decisions may also be "overridden" because calls are not
     uniquely identified. *)
  let add_call_decision call round (decision : Inlining_stats_types.Decision.t) =
    match call.decision with
    | Decision(round', _) when round' > round -> call
    | _ -> { call with decision = Decision (round, decision) }

  let add_decision stack (round, decision) t : node Place_map.t =
    let rec loop (t : t) (stack : IH.History.t) =
      match stack with
      | [] -> t
      | Inlined :: _ -> assert false
      | Specialised :: _ -> assert false
      | SpecialisedCall :: _ -> assert false
      | node :: rest ->
        let atom = IH.node_to_atom node in
        match (node : IH.History.atom) with
        | Closure _ ->
          let v =
              match Place_map.find atom t with
              | Closure v -> v
              | _ -> assert false
              | exception Not_found -> Place_map.empty
          in
          let v = loop v rest in
          Place_map.add atom (Closure v) t
        | Module _ ->
          let v =
              match Place_map.find atom t with
              | Module v -> v
              | _ -> assert false
              | exception Not_found -> Place_map.empty
          in
          let v = loop v rest in
          Place_map.add atom (Module v) t
        | Call (_, _, path) ->
          let v =
              match Place_map.find atom t with
              | Call v -> v
              |_ -> assert false
              | exception Not_found -> empty_call path
          in
          let merge_call_annotation annotation rest =
            let annotation =
              match annotation with
              | None -> Place_map.empty
              | Some x -> x
            in
            loop annotation rest
          in
          let v =
            match rest with
            | [] ->
              add_call_decision v round decision
            | Inlined :: rest ->
              let inlined = merge_call_annotation v.inlined rest in
              { v with inlined = Some inlined }
            | Specialised :: rest ->
              let specialised = merge_call_annotation v.specialised rest in
              { v with specialised = Some specialised }
            | SpecialisedCall :: rest ->
              let specialised_call =
                merge_call_annotation v.specialised_call rest
              in
              { v with specialised_call = Some specialised_call }
            | Call _ :: _ -> assert false
            | Closure _ :: _ -> assert false
            | Module _ :: _ -> assert false
          in
          Place_map.add atom (Call v) t
        | _ -> assert(false)
    in
    loop t (List.rev stack)

  let get_external_logs modname =
    if Hashtbl.mem external_logs modname then
      Hashtbl.find external_logs modname
    else
      let filename =
        try
          Some (Misc.find_in_path_uncap
                  !Config.load_path
                  (modname ^ ".inlining.org"))
        with Not_found ->
          None
      in
      Hashtbl.add external_logs modname filename;
      filename

  let build log =
    Hashtbl.fold add_decision log Place_map.empty

  (* build a string representing a link in the org file. Takes
     into account external files *)
  let build_link_uid inlining_report_file path =
    let link =
      if IH.Path.file path <> Flambda.current_module () then
        let filename = get_external_logs (IH.Path.file path) in
        match filename with
        | None -> None
        | Some filename ->
          Some ("file:" ^
                Location.find_relative_path_from_to inlining_report_file filename ^
                "::")
      else
        Some ("")
    in
    match link with
    | None -> None
    | Some p ->
      Some (p ^ (IH.Path.to_uid path))

  let print_stars ppf n =
    let s = String.make n '*' in
    Format.fprintf ppf "%s" s

  let print_reference inlining_report_file ppf reference =
    let location_short =
      if IH.Path.file reference <> Flambda.current_module () then
        "module " ^ (IH.Path.file reference)
      else
        "the current file"
    in
    let link =
      match build_link_uid inlining_report_file reference with
      | None -> ""
      | Some x ->
        Format.asprintf " at this [[%s][decision site]]" x
    in
    Format.fprintf ppf "The decision to inline this call was taken in %s%s."
      location_short link

  let print_anchor ppf anchor =
    Format.fprintf ppf "[[id:<<%s>>][ ]]" anchor

  let print_apply def inlining_report_file ppf name =
    let link = build_link_uid inlining_report_file name in
    match link with
    | Some link ->
    Format.fprintf ppf "[[%s][%a]]"
      link
      IH.Definition.print_short def
    | None ->
      Format.fprintf ppf "%a"
      IH.Definition.print_short def

  let print_debug ppf (dbg : Debuginfo.item) =
    if Debuginfo.is_none_item dbg then ()
    else Format.fprintf ppf "%s" (Debuginfo.to_string [dbg])

  let rec print history filename ~depth ppf t =
    Place_map.iter (fun atom (v : node) ->
      let present = IH.Path.append_atom atom history in
      let uid = IH.Path.to_uid present in
      let print_checkpoint tag name dbg =
         Format.fprintf ppf "@[<h>%a %s %s%a %a@]@;@;"
           print_stars (depth + 1)
           tag
           name
           print_debug dbg
           print_anchor uid;
      in
      let print_application name dbg converter obj =
        let def =
          (*IH.Path.get_compressed_path history name
            |>*)
          Inlining_history.path_to_definition (Flambda.current_module ()) name
        in
        Format.pp_open_vbox ppf (depth + 2);
        Format.fprintf ppf "@[<h>%a Application of %a%a %a@]@;@;\
                            @[%a@]@;@;@[%a@]"
          print_stars (depth + 1)
          (print_apply def filename) name
          print_debug dbg
          print_anchor uid
          Inlining_history.Definition.print def
          converter obj;
        Format.pp_close_box ppf ();
        Format.pp_print_newline ppf ();
        Format.pp_print_newline ppf ();
      in
      begin
        match atom, v with
       | Module(name, dbg), Module t ->
         print_checkpoint "Module" name dbg;
         print present filename ppf ~depth:(depth + 1) t;
       | Closure(name, dbg), Closure t ->
         print_checkpoint "Definition" (IH.string_of_name name) dbg;
         print present filename ppf ~depth:(depth + 1) t;
       | Call(path, dbg), Call c ->
         begin
           match c.decision with
           | Reference reference ->
             begin
               print_application path dbg
                 (print_reference filename) reference;
               let explore entry next_atom =
                 match entry with
                 | None -> ()
                 | Some specialised ->
                   print (IH.Path.append_atom next_atom present) filename ppf
                     ~depth:(depth + 1) specialised
               in
               explore c.specialised IH.Path.Specialised;
               explore c.specialised_call IH.Path.SpecialisedCall;
               explore c.inlined IH.Path.Inlined;
             end
           | Decision (round, decision) ->
             begin
               print_application path dbg
                 (Inlining_stats_types.Decision.summary round) decision;
               Inlining_stats_types.Decision.print
                 ~specialised:(c.specialised,
                               IH.Path.append_atom IH.Path.Specialised present)
                 ~inlined:(c.inlined,
                           IH.Path.append_atom IH.Path.Inlined present)
                 ~print:(fun (x : IH.Path.t) -> print x filename)
                 ~depth:(depth + 1) ppf decision
                 ~specialised_call:(c.specialised_call,
                                    IH.Path.append_atom
                                      IH.Path.SpecialisedCall present)
             end
         end
       | _ -> assert false
     end;
      if depth = 0 then Format.pp_print_newline ppf ())
      t

  let print ppf t filename =
    print (IH.Path.empty (Flambda.current_module ())) ~depth:0 filename ppf t
end

let really_save_then_forget_decisions ~output_prefix =
  let report = Inlining_report.build log in
  let filename = (output_prefix ^ ".inlining.org") in
  let out_channel = open_out filename in
  let ppf = Format.formatter_of_out_channel out_channel in
  Inlining_report.print ppf report filename;
  close_out out_channel

let save_then_forget_decisions ~output_prefix =
  if !Clflags.inlining_report then begin
    really_save_then_forget_decisions ~output_prefix;
    Hashtbl.clear log;
    Hashtbl.clear external_logs
  end

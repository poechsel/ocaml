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

let log
  : (Inlining_history.t * Inlining_stats_types.Decision.t) list ref
  = ref []

let record_decision decision ~closure_stack =
  if !Clflags.inlining_report then begin
    match closure_stack with
    | []
    | Inlining_history.Module _ :: _
    | Inlining_history.Inlined :: _
    | Inlining_history.Specialised _ :: _ ->
      Misc.fatal_errorf "record_decision: missing Call node"
    | Inlining_history.Closure _ :: _
    | Inlining_history.Call _ :: _ ->
      log := (closure_stack, decision) :: !log
  end

module Inlining_report = struct

  module Place = struct
    type kind =
      | Closure
      | Module
      | Call

    type t = Debuginfo.t * string * Inlining_history.path * kind

    let compare ((d1, cl1, _, k1) : t) ((d2, cl2, _, k2) : t) =
      let c = Debuginfo.compare d1 d2 in
      if c <> 0 then c else
      let c = Pervasives.compare cl1 cl2 in
      if c <> 0 then c else
        match k1, k2 with
        | Closure, Closure -> 0
        | Call, Call -> 0
        | Module, Module -> 0
        | Closure, Call -> 1
        | Call, Closure -> -1
        | Module, _ -> 1
        | _, Module -> -1
  end

  module Place_map = Map.Make(Place)

  type decision =
    | Decision of Inlining_stats_types.Decision.t
    | Reference of Inlining_history.path

  type t = (node * string) Place_map.t

  and node =
    | Closure of t
    | Module of t
    | Call of call

  and call =
    { decision: decision;
      inlined: t option;
      specialised: t option; }

  let empty_call path =
    { decision = Reference path;
      inlined = None;
      specialised = None; }


  (* Prevented or unchanged decisions may be overridden by a later look at the
     same call. Other decisions may also be "overridden" because calls are not
     uniquely identified. *)
  let add_call_decision call (decision : Inlining_stats_types.Decision.t) =
    match call.decision, decision with
    | Reference _, _ -> { call with decision = Decision decision }
    | Decision Definition, _ -> { call with decision = Decision decision }
    | _, Definition -> call
    | Decision _, Prevented _ -> call
    | Decision (Prevented _), _ -> { call with decision = Decision decision }
    | Decision (Specialised _), _ -> call
    | Decision _, Specialised _ -> { call with decision = Decision decision }
    | Decision (Inlined _), _ -> call
    | Decision _, Inlined _ -> { call with decision = Decision decision }
    | Decision Unchanged _, Unchanged _ -> call

  let add_decision t (stack, decision) : (node * string) Place_map.t =
    let debug_empty = Inlining_history.empty_path in
    let rec loop seen t (stack : Inlining_history.t) =
      let uid seen =
        Inlining_history.uid_of_path (Inlining_history.history_to_path seen)
      in
      match stack with
      | (Closure(cl, dbg) as x) :: rest ->
          let key : Place.t = (dbg, Inlining_history.string_of_name cl, debug_empty, Closure) in
          let v =
            try
              match Place_map.find key t with
              | Closure v, _ -> v
              | _ -> assert false
            with Not_found -> Place_map.empty
          in
          let seen = x :: seen in
          let v = loop seen v rest in
          Place_map.add key (Closure v, uid seen) t
      (* CR poechsel: deal with modules params *)
      | (Module(s, dbg, _) as x) :: rest ->
          let key : Place.t = (dbg, s, debug_empty, Module) in
          let v =
            try
              match Place_map.find key t with
              | Module v, _ -> v
              | _ -> assert false
            with Not_found -> Place_map.empty
          in
          let seen = x :: seen in
          let v = loop seen v rest in
          Place_map.add key (Module v, uid seen) t
      | (Call(name, dbg, path) as x) :: rest ->
          let key : Place.t = (dbg, Format.asprintf "%a" Inlining_history.print name, name, Call) in
          let v =
            try
              match Place_map.find key t with
              | Call v, _ -> v
              |_ -> assert false
            with Not_found -> empty_call path
          in
          let v, seen =
            match rest with
            | [] -> add_call_decision v decision, x :: seen
            | (Inlined as y) :: rest ->
                let inlined =
                  match v.inlined with
                  | None -> Place_map.empty
                  | Some inlined -> inlined
                in
                let seen = y :: x :: seen in
                let inlined = loop seen inlined rest in
                { v with inlined = Some inlined }, seen
            | (Specialised _ as y) :: rest ->
                let specialised =
                  match v.specialised with
                  | None -> Place_map.empty
                  | Some specialised -> specialised
                in
                let seen = y :: x :: seen in
                let specialised = loop seen specialised rest in
                { v with specialised = Some specialised }, seen
            | Call _ :: _ -> assert false
            | Closure _ :: _ -> assert false
            | Module _ :: _ -> assert false
          in
          Place_map.add key (Call v, uid seen) t
      | [] -> t
      | Inlined :: _ -> assert false
      | Specialised _ :: _ -> assert false
    in
    loop [] t (List.rev stack)

  let build log =
      List.fold_left add_decision Place_map.empty log

  let print_stars ppf n =
    let s = String.make n '*' in
    Format.fprintf ppf "%s" s

  let print_reference ppf reference =
    Inlining_history.uid_of_path reference
    |> Format.fprintf ppf "The decision for this site was taken at:@;  [[%s][decision site]]"

  let print_anchor ppf anchor =
    Format.fprintf ppf "[[id:<<%s>>][ ]]" anchor

  let rec print ~depth ppf t =
    Place_map.iter (fun (dbg, cl, name, _) (v, uid) ->
       match v with
       | Module t ->
         Format.fprintf ppf "@[<h>%a Module %s%s %a@]@."
           print_stars (depth + 1)
           cl
           (Debuginfo.to_string dbg)
           print_anchor uid;
         print ppf ~depth:(depth + 1) t;
         if depth = 0 then Format.pp_print_newline ppf ()
       | Closure t ->
         Format.fprintf ppf "@[<h>%a Definition of %s%s %a@]@."
           print_stars (depth + 1)
           cl
           (Debuginfo.to_string dbg)
           print_anchor uid;
         print ppf ~depth:(depth + 1) t;
         if depth = 0 then Format.pp_print_newline ppf ()
       | Call c ->
         match c.decision with
         | Reference reference ->
           begin
           Format.pp_open_vbox ppf (depth + 2);
           Format.fprintf ppf "@[<h>%a Application of [[%s][%a]]%s %a@]@;@;@[%a@]"
             print_stars (depth + 1)
             (Inlining_history.uid_of_path name)
             Inlining_history.print name
             (Debuginfo.to_string dbg)
             print_anchor uid
             print_reference reference;
           Format.pp_close_box ppf ();
           Format.pp_print_newline ppf ();
           Format.pp_print_newline ppf ();
           begin
             match c.specialised with
             | None -> ()
             | Some specialised ->
               print ppf ~depth:(depth + 1) specialised
           end;
           begin
             match c.inlined with
             | None -> ()
             | Some inlined ->
               print ppf ~depth:(depth + 1) inlined
           end;
             end
         | Decision decision ->
           Format.pp_open_vbox ppf (depth + 2);
           Format.fprintf ppf "@[<h>%a Application of [[%s][%a]]%s %a@]@;@;@[%a@]"
             print_stars (depth + 1)
             (Inlining_history.uid_of_path name)
             Inlining_history.print name
             (Debuginfo.to_string dbg)
             print_anchor uid
             Inlining_stats_types.Decision.summary decision;
           Format.pp_close_box ppf ();
           Format.pp_print_newline ppf ();
           Format.pp_print_newline ppf ();
           Inlining_stats_types.Decision.meta_print
             ~specialised:c.specialised ~inlined:c.inlined
             ~print:print ~depth:(depth + 1)  ppf decision;
           if depth = 0 then Format.pp_print_newline ppf ())
      t

  let print ppf t = print ~depth:0 ppf t

end

let really_save_then_forget_decisions ~output_prefix =
  let report = Inlining_report.build !log in
  let out_channel = open_out (output_prefix ^ ".inlining.org") in
  let ppf = Format.formatter_of_out_channel out_channel in
  Inlining_report.print ppf report;
  close_out out_channel

let save_then_forget_decisions ~output_prefix =
  if !Clflags.inlining_report then begin
    really_save_then_forget_decisions ~output_prefix
  end

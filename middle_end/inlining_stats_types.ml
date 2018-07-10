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

module Wsb = Inlining_cost.Whether_sufficient_benefit

let print_stars ppf n =
  let s = String.make n '*' in
  Format.fprintf ppf "%s" s

let print_calculation ~depth ~title ~subfunctions ppf wsb =
  Format.pp_open_vbox ppf (depth + 2);
  Format.fprintf ppf "@[<h>%a %s@]@;@;@[%a@]"
    print_stars (depth + 1)
    title
    (Wsb.print_description ~subfunctions) wsb;
  Format.pp_close_box ppf ();
  Format.pp_print_newline ppf ();
  Format.pp_print_newline ppf ()

module type Log_intf = sig
  type t
  val calculation : depth:int -> Format.formatter -> t -> unit
  val need_precisions : t -> bool
end

module Inlined = struct

  type t =
    | Classic_mode
    | Annotation
    | Decl_local_to_application
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t

  let summary round ppf = function
    | Classic_mode ->
      Format.pp_print_text ppf @@
      "This function was inlined at round " ^
      string_of_int round ^
      " because it was small enough to be inlined in `-Oclassic'"
    | Annotation ->
      Format.pp_print_text ppf @@
      "This function was inlined at round " ^
      string_of_int round ^
      " because of an annotation."
    | Decl_local_to_application ->
      Format.pp_print_text ppf @@
      "This function was inlined at round " ^
      string_of_int round ^
      " because it was local to this application."
    | Without_subfunctions _ ->
      Format.pp_print_text ppf @@
      "This function was inlined at round " ^
      string_of_int round ^
      " because the expected benefit outweighed the change in code size."
    | With_subfunctions _ ->
      Format.pp_print_text ppf @@
      "This function was inlined at round " ^
      string_of_int round ^
      " because the expected benefit outweighed the change in code size."

  let calculation ~depth ppf = function
    | Classic_mode -> ()
    | Annotation -> ()
    | Decl_local_to_application -> ()
    | Without_subfunctions wsb ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:false ppf wsb
    | With_subfunctions(_, wsb) ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:true ppf wsb

  let need_precisions = function
    | Classic_mode | Annotation | Decl_local_to_application
      -> false
    | Without_subfunctions _ | With_subfunctions _
      -> true

end

module Not_inlined = struct
  type t =
    | Classic_mode
    | Above_threshold of int
    | Annotation
    | No_useful_approximations
    | Inlining_depth_exceeded
    | Unrolling_depth_exceeded
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t


  let summary _round ppf = function
    | Classic_mode ->
      Format.pp_print_text ppf
        "This function was not inlined because it was too \
         large to be inlined in `-Oclassic'."
    | Above_threshold size ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         it was larger than the current size threshold";
        Format.fprintf ppf "(%i)" size
    | Annotation ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         of an annotation."
    | No_useful_approximations ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         there was no useful information about any of its parameters, \
         and it was not particularly small."
    | Inlining_depth_exceeded ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         the inlining depth was exceeded."
    | Unrolling_depth_exceeded ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         its unrolling depth was exceeded."
    | Without_subfunctions _ ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         the expected benefit did not outweigh the change in code size."
    | With_subfunctions _ ->
      Format.pp_print_text ppf
        "This function was not inlined because \
         the expected benefit did not outweigh the change in code size."

  let calculation ~depth ppf = function
    | Classic_mode
    | Above_threshold _
    | Annotation
    | No_useful_approximations
    | Inlining_depth_exceeded
    | Unrolling_depth_exceeded -> ()
    | Without_subfunctions wsb ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:false ppf wsb
    | With_subfunctions(_, wsb) ->
      print_calculation
        ~depth ~title:"Inlining benefit calculation"
        ~subfunctions:true ppf wsb

  let need_precisions = function
    | Classic_mode | Above_threshold _ | Annotation
    | No_useful_approximations | Inlining_depth_exceeded
    | Unrolling_depth_exceeded
      -> false
    | Without_subfunctions _ | With_subfunctions _
      -> true

end

module Specialised = struct
  type t =
    | Annotation
    | Without_subfunctions of Wsb.t
    | With_subfunctions of Wsb.t * Wsb.t

  let summary round ppf = function
    | Annotation ->
      Format.pp_print_text ppf @@
      "This function was specialised at round " ^
      string_of_int round ^
      " because of an annotation."
    | Without_subfunctions _ ->
      Format.pp_print_text ppf @@
      "This function was specialised at round " ^
      string_of_int round ^
      " because the expected benefit outweighed the change in code size."
    | With_subfunctions _ ->
      Format.pp_print_text ppf @@
        "This function was specialised at round " ^
      string_of_int round ^
      " because the expected benefit \
         outweighed the change in code size."


  let calculation ~depth ppf = function
    | Annotation -> ()
    | Without_subfunctions wsb ->
        print_calculation
          ~depth ~title:"Specialising benefit calculation"
          ~subfunctions:false ppf wsb
    | With_subfunctions(_, wsb) ->
        print_calculation
          ~depth ~title:"Specialising benefit calculation"
          ~subfunctions:true ppf wsb

  let need_precisions = function
    | Annotation
      -> false
    | Without_subfunctions _ | With_subfunctions _
      -> true
end

module Not_specialised = struct
  type t =
    | Classic_mode
    | Above_threshold of int
    | Annotation
    | Not_recursive
    | Not_closed
    | No_invariant_parameters
    | No_useful_approximations
    | Specialised_depth_exceeded
    | Not_beneficial of Wsb.t * Wsb.t

  let summary _round ppf = function
    | Classic_mode ->
      Format.pp_print_text ppf
        "This function was not specialised because it was \
         compiled with `-Oclassic'."
    | Above_threshold size ->
      Format.pp_print_text ppf
        "This function was not specialised because \
         it was larger than the current size threshold";
        Format.fprintf ppf "(%i)" size
    | Annotation ->
      Format.pp_print_text ppf
        "This function was not specialised because \
         of an annotation."
    | Not_recursive ->
      Format.pp_print_text ppf
        "This function was not specialised because \
         it is not recursive."
    | Not_closed ->
      Format.pp_print_text ppf
        "This function was not specialised because \
         it is not closed."
    | No_invariant_parameters ->
      Format.pp_print_text ppf
        "This function was not specialised because \
          it has no invariant parameters."
    | No_useful_approximations ->
      Format.pp_print_text ppf
        "This function was not specialised because \
         there was no useful information about any of its invariant \
         parameters."
    | Not_beneficial _ ->
      Format.pp_print_text ppf
        "This function was not specialised because \
          the expected benefit did not outweigh the change in code size."
    | Specialised_depth_exceeded ->
      Format.pp_print_text ppf
        "This function was not specialised because \
          the specialisation depth was exceeded."

  let calculation ~depth ppf = function
    | Classic_mode
    | Above_threshold _
    | Annotation
    | Not_recursive
    | Not_closed
    | No_invariant_parameters
    | Specialised_depth_exceeded
    | No_useful_approximations -> ()
    | Not_beneficial(_, wsb) ->
      print_calculation
        ~depth ~title:"Specialising benefit calculation"
        ~subfunctions:true ppf wsb

  let need_precisions = function
    | Classic_mode | Above_threshold _ | Annotation | Not_recursive
    | Not_closed | No_invariant_parameters | Specialised_depth_exceeded
    | No_useful_approximations
      -> false
    | Not_beneficial _
      -> true
end

module Prevented = struct
  type t =
    | Function_prevented_from_inlining
    | Level_exceeded

  let summary _round ppf = function
    | Function_prevented_from_inlining ->
      Format.pp_print_text ppf
        "This function was prevented from inlining or specialising."
    | Level_exceeded ->
      Format.pp_print_text ppf
        "This function was prevented from inlining or specialising \
         because the inlining depth was exceeded."
end

module Decision = struct
  type t =
    | Definition
    | Prevented of Prevented.t
    | Specialised of Specialised.t
    | Inlined of Not_specialised.t * Inlined.t
    | Unchanged of Not_specialised.t * Not_inlined.t

  let summary round ppf = function
    | Definition -> ()
    | Prevented p ->
      Prevented.summary round ppf p
    | Specialised s ->
      Specialised.summary round ppf s
    | Inlined (s, i) ->
      Format.fprintf ppf "@[<v>@[%a@]@;@;@[%a@]@]"
        (Not_specialised.summary round) s (Inlined.summary round) i
    | Unchanged (s, i) ->
      Format.fprintf ppf "@[<v>@[%a@]@;@;@[%a@]@]"
        (Not_specialised.summary round) s (Not_inlined.summary round) i

  let calculation ~depth ppf = function
    | Definition -> ()
    | Prevented _ -> ()
    | Specialised s ->
      Specialised.calculation ~depth ppf s
    | Inlined (s, i) ->
      Not_specialised.calculation ~depth ppf s;
      Inlined.calculation ~depth ppf i
    | Unchanged (s, i) ->
      Not_specialised.calculation ~depth ppf s;
      Not_inlined.calculation ~depth ppf i

  let print_decision (type a) ppf tag
        (module Log : Log_intf with type t=a) (decision:a)
        entry ~print ~depth =
    if Log.need_precisions decision || entry <> None then begin
      Format.fprintf ppf "@[<h>%a %s@]@."
        print_stars (depth + 1) tag;
      Log.calculation ~depth:(depth+1) ppf decision;
      match entry with
      | None -> ()
      | Some s -> print ~depth:(depth+1) ppf s
    end

  let print ~depth ppf ~specialised ~inlined
        ~specialised_call
        ~print
        decision =
    let specialised, specialised_hist = specialised in
    let inlined, inlined_hist = inlined in
    let specialised_call, specialised_call_hist = specialised_call in
    match decision with
    | Definition ->
      ()
    | Prevented _ ->
      ()
    | Specialised s ->
      begin
        print_decision ppf "Specialisation" ~depth (module Specialised)
          s specialised ~print:(print specialised_hist);

        if specialised_call <> None then begin
          match specialised_call with
          | None -> ()
          | Some s -> print specialised_call_hist ~depth:(depth+1) ppf s
        end
      end
    | Inlined(s, i) ->
      begin
        print_decision ppf "Speculative specialisation" ~depth
          (module Not_specialised) s specialised
          ~print:(print specialised_hist);
        print_decision ppf "Inlined" ~depth (module Inlined) i inlined
          ~print:(print inlined_hist);
      end
    | Unchanged (s, i) ->
      begin
        print_decision ppf "Speculative specialisation" ~depth
          (module Not_specialised)
          s specialised ~print:(print specialised_hist);
        print_decision ppf "Speculative inlining" ~depth (module Not_inlined)
          i inlined ~print:(print inlined_hist);
      end

end

(* TEST
<<<<<<< HEAD
modules = "thread_exit_in_callback_stub.c"
exit_status = "42"
=======
>>>>>>> ocaml/4.12
* hassysthreads
include systhreads
** bytecode
** native
*)

<<<<<<< HEAD
(* We cannot tell Ocamltest that this program is supposed to stop with
   a fatal error. Instead, we install a fatal error hook and call exit(42) *)
external install_fatal_error_hook : unit -> unit = "install_fatal_error_hook"

let _ =
  install_fatal_error_hook ();
  Gc.Memprof.(start ~callstack_size:10 ~sampling_rate:1.
    { null_tracker with alloc_minor = fun _ -> Thread.exit (); None });
  ignore (Sys.opaque_identity (ref 1))
=======
let _ =
  let main_thread = Thread.id (Thread.self ()) in
  Gc.Memprof.(start ~callstack_size:10 ~sampling_rate:1.
                { null_tracker with alloc_minor = fun _ ->
                      if Thread.id (Thread.self ()) <> main_thread then
                        Thread.exit ();
                      None });
  let t = Thread.create (fun () ->
      ignore (Sys.opaque_identity (ref 1));
      assert false) ()
  in
  Thread.join t;
  Gc.Memprof.stop ()

let _ =
  Gc.Memprof.(start ~callstack_size:10 ~sampling_rate:1.
    { null_tracker with alloc_minor = fun _ -> Thread.exit (); None });
  ignore (Sys.opaque_identity (ref 1));
  assert false
>>>>>>> ocaml/4.12

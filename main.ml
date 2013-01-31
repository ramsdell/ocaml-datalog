(** A main routine for a Datalog prover. *)

open Prover

open Reader

(** This section is all for argument processing *)

let get_args() =

  (* file names *)

  (** defaults to stdout *)
  let output_file = ref "-" in

  (** defaults to stdin *)
  let input_file = ref "-" in

  (** [anon_var] is used to track the number of free-floating
  arguments.  0 means we have seen none, 1 means we have seen 1, 2
  means we have seen too many. *)

  let anon_var = ref 0 in

  let anon_fun str =
    match !anon_var with
      0 ->
	input_file := str;
	anon_var := 1
    | _ ->
	anon_var := 2 in

  (* set up Arg.parse *)

  let arg_spec =
    [("-o", Arg.String(fun s -> output_file := s),
      "name - send output to this file (default stdout)" );
     ("--", Arg.Rest(anon_fun),
      "     - treat remaining args as file names, where - means stdin")] in

  let usage_msg = "Usage: datalog  [-o output] [input]" in

  Arg.parse arg_spec anon_fun usage_msg;

  if !anon_var > 1 then
    begin
      prerr_string "bad arg count";
      prerr_newline();
      prerr_string usage_msg;
      prerr_newline();
      exit 1
    end;
  !input_file, !output_file

let run (input_file_name, output_file_name) =
  try

    (** read input *)
    let (clauses, atom) = Reader.read_program input_file_name in

    (** load assumptions *)
    let theory = create 128 in

    List.iter (assume theory) clauses;

    (** run query *)
    let atoms = prove theory atom in

    (** open output file *)
    let out =
      if output_file_name = "-" then
	stdout
      else
	open_out output_file_name in

    Format.set_formatter_out_channel out;

    (** display output *)
    let show atom =
      print_atom atom;
      Format.print_string ".";
      Format.print_newline() in

    List.iter show atoms;

    exit 0

  with Failure s ->
    prerr_string s;
    prerr_newline();
    exit 1

let _ =
  run (get_args())

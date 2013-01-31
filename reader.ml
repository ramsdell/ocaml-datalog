(* A reader for Datalog programs. *)

(* Much of this code is dedicated to showing the location of an error
   in the input. *)

let report_error file_name lexbuf msg =
  let (lineno, start, finish) =
    Scanner.lexeme_place lexbuf in
  let msg =
    Printf.sprintf
      "File \"%s\", line %d, characters %d-%d: %s"
      file_name lineno start finish msg in
  failwith msg

let report_parse_error file_name lexbuf =
  report_error file_name lexbuf "syntax error"

let open_in_or_stdin file_name =
  if file_name = "-" then
    stdin
  else
    open_in file_name

(* Reads a Datalog program.  The argument is the name of the file.
   Standard input is used if the file name is "-". *)
let read_program file_name =
  let chan = open_in_or_stdin file_name in
  let lexbuf = Lexing.from_channel chan in
  try
    let prog =
      Parser.program Scanner.token lexbuf in
    close_in chan;
    prog
  with Parsing.Parse_error ->
    close_in chan;
    report_parse_error file_name lexbuf
  | Failure s ->
    close_in chan;
    report_error file_name lexbuf s

let read_clause_from_string str =
  let lexbuf = Lexing.from_string str in
  Scanner.init();
  try
    Parser.a_clause Scanner.token lexbuf
  with Parsing.Parse_error ->
    report_parse_error "-" lexbuf
  | Failure s ->
    report_error "-" lexbuf s

(* A lexical analyzer for Datalog input. *)

{

open Parser

(* Definitions used to report the location of errors. *)
let line_number = ref 1
let line_start = ref 0
let newline_found lexbuf =
  line_number := !line_number + 1;
  line_start := Lexing.lexeme_end lexbuf
let init() =
  line_number := 1;
  line_start := 0
let lexeme_place lexbuf =
  let start =
    Lexing.lexeme_start lexbuf - !line_start + 1 in
  let finish =
    Lexing.lexeme_end lexbuf - !line_start + 1 in
  (!line_number, start, finish)

(* Quoted string handling *)

(* Convert the external version of string into the internal representation *)
let strip str =
  Scanf.sscanf str "%S" (fun s -> s)

}

let newline = ('\n' | '\r' | "\r\n")
let var_start = ['A'-'Z' '_']
let val_start = ['a'-'z' '0'-'9']
let part = ['A'-'Z' 'a'-'z' '_' '0'-'9']

let identifier = var_start part*
let value = val_start part*
let str_chars = [^ '"' '\\'] | "\\\\" | "\\\""  | "\\'"
| "\\n" | "\\r" | "\\t" | "\\b"
| "\\" [ '0'-'9' ]  [ '0'-'9' ]  [ '0'-'9' ]
let str = '"' str_chars* '"'

rule token = parse
  [' ' '\t']                { token lexbuf }
| newline                   { newline_found lexbuf; token lexbuf }
  (* skip comments starting with the percent sign '%' *)
| '%' [^ '\r' '\n' ]* newline { newline_found lexbuf; token lexbuf }
| '('                       { LPAREN }
| ')'                       { RPAREN }
| ','                       { COMMA }
| '?'                       { QUESTION }
| '.'                       { PERIOD }
| '='                       { EQUAL }
| ":-"                      { IMPLY }
| identifier                { VAR(Lexing.lexeme lexbuf) }
| value                     { VAL(Lexing.lexeme lexbuf) }
| str                       { VAL(strip(Lexing.lexeme lexbuf)) }
| eof                       { EOF }

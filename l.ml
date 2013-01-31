(* Use me to test datalog and the reader. *)
#load "datalog.cmo";;
#load "prover.cmo";;
#load "parser.cmo";;
#load "scanner.cmo";;
#load "reader.cmo";;
open Prover;;
#install_printer print_atom;;
#install_printer print_clause;;

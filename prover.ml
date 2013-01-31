open Format

include Datalog.Make(struct
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end)

let inrange x low high = (x >= low) && (x <= high)
let isupper c = inrange c 'A' 'Z'
let islower c = inrange c 'a' 'z'
let isnum c = inrange c '0' '9'
let isalphanum c = isupper c || islower c || isnum c
let isvalstart c = islower c || isnum c
let isvalpart c = c = '_' || isalphanum c

let isval string =
  String.length string > 0 && isvalstart(string.[0]) &&
  let rec loop i =
    if i >= String.length string then
      true
    else if isvalpart(string.[i]) then
      loop (i + 1)
    else
      false in
  loop 1

let print_value string =
  if isval string then
    print_string string
  else
    print_string (Printf.sprintf "%S" string)

let print_term term =
  spreadterm print_string print_value term

let print_atom atom =
  let pred = getpred atom in
  let terms = getterms atom in
  match pred, terms with
    "=", [a; b] ->			(* print equality *)
      open_box 2;
      print_term a;
      print_space();
      print_string "= ";
      print_term b;
      close_box()
  | _ ->				(* print predicate *)
      print_string pred;
      match terms with
	[] -> ()
      | term :: terms ->
	  print_char '(';
	  open_box 0;
	  print_term term;
	  let print_rest term =
	    print_char ',';
	    print_space();
	    print_term term in
	  List.iter print_rest terms;
	  close_box();
	  print_char ')'

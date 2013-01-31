(* An implementation of Datalog.
   Copyright (C) 2005 The MITRE Corporation

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* The inference engine uses tabled logic programming to ensure that
   all queries terminate. *)

(* See the interface for comments *)
module type DatalogType = Hashtbl.HashedType

(* See the interface for comments *)
module type T =
  sig
    type value
    type term
    val mkvar : string -> term
    val mkval : value -> term
    val spreadterm : (string -> 'a) -> (value -> 'a) -> term -> 'a
    type literal
    val mkliteral : string -> term list -> literal
    val getpred : literal -> string
    val getterms : literal -> term list
    type clause
    val mkclause : literal -> literal list -> clause
    val gethead : clause -> literal
    val getbody : clause -> literal list
    type primitive = int -> value list -> value list option
    val add_primitive : string -> int -> primitive -> unit
    type theory
    val create : int -> theory
    val copy : theory -> theory
    exception Unsafe_clause
    val assume : theory -> clause -> unit
    val retract : theory -> clause -> unit
    val prove : theory -> literal -> literal list
  end

module Make(D: DatalogType):
    T with type value = D.t =
  struct
    module Stringtbl =
      Weak.Make(struct
	type t = string
	let equal = (=)
	let hash = Hashtbl.hash
      end)

(* table used to intern varibles and predicate names *)
    let stringtbl = Stringtbl.create 101

    let intern string =
      Stringtbl.merge stringtbl string

    let same = intern "="

    type value = D.t

    type variable = string

    type term =
	Var of variable
      | Val of value

    let eqv (v: variable) (v': variable) = v == v'

    let eqt (t: term) (t': term) =
      match t, t' with
	Var v, Var v' -> eqv v v'
      | Val v, Val v' -> D.equal v v'
      | _ -> false

    let mkvar id = Var (intern id)

    let mkval v = Val v

    let spreadterm getvar getval term =
      match term with
	Var v -> getvar v
      | Val v -> getval v

(* variables *)

    let id = ref 0

(* generates fresh variables *)
(* assumes normal variables are not numbers *)
    let fresh () =
      let n = !id in
      id := n + 1;
      Var (string_of_int n)		(* don't intern fresh vars *)

(* enviroments *)

    type binding = variable * term

    type env = binding list

    let extend env var term =
      (var, term) :: env

    let rec lookup env var =
      match env with
	[] -> None
      | (var', term) :: env ->
	  if eqv var var' then
	    Some term
	  else
	    lookup env var

(* literals *)

    type predicate = string

    type literal = predicate * term list

    let eqp (p: predicate) (p': predicate) = p == p'

    let hash_pred (p: predicate) = Hashtbl.hash p

    let mkliteral pred terms = intern pred, terms

    let getpred (pred, terms) = pred

    let getterms (pred, terms) = terms

(* variant checking between literals *)

(* Two literals are variants of each other if there is a one-to-one
   mapping of variables such that the substitions defined by the map
   transform one literal into the other. *)

(* This routine constructs a map and its inverse to ensure the map is
   one-to-one. *)
    let rec variant_terms env env' terms terms' =
      match terms, terms' with
	[], [] -> true
      | term :: terms, term' :: terms' ->
	  variant_term env env' term term' terms terms'
      | _, _ -> false
    and variant_term env env' term term' terms terms' =
      match term, term' with
	Var var, Var var' ->
	  variant_var env env' var var' terms terms'
      | _, _ ->
	  eqt term term' && variant_terms env env' terms terms'
    and variant_var env env' var var' terms terms' =
      match lookup env var, lookup env' var' with
	None, None ->
	  let env = extend env var (Var var') in
	  let env' = extend env' var' (Var var) in
	  variant_terms env env' terms terms'
      | Some (Var v), Some (Var v') ->
	  eqv v var' && eqv v' var && variant_terms env env' terms terms'
      | _, _ -> false

    let variant (pred, terms) (pred', terms') =
      if not (eqp pred pred') then
	false
      else
	variant_terms [] [] terms terms'

    let rec mem_literal literal list =
      match list with
	[] -> false
      | literal' :: list' -> variant literal literal' ||
	mem_literal literal list'

(* A hash function for literals that respects variants *)

(* A variable is always hashed to the same number to ensure that the
   hash function respects variants *)
    let hash_term term =
      match term with
	Var var -> 101
      | Val value -> D.hash value

    let hash_literal (pred, terms) =
      let rec loop code i terms =
	match terms with
	  [] -> code
	| term :: terms ->
	    let code = code + (hash_term term) - i * 7 in
	    loop code (i + 1) terms in
      loop (hash_pred pred) 0 terms

(* Literal tables -- tables with literals as keys, where literals are
   considered the same if one is a variant of the other. *)

    module Literaltbl =
      Hashtbl.Make(struct
	type t = literal
	let equal = (variant : literal -> literal -> bool)
	let hash = (hash_literal : literal -> int)
      end)

(* substitution *)

(* substitute a value for variable in a term if it is bound in the
   environment *)
    let subst_term env term =
      match term with
	Var var ->
	  (match lookup env var with
	    None -> term
	  | Some term' -> term')
      | _ -> term

(* substitute values for variables in a literal *)
    let subst_literal env (pred, terms) =
      (pred, List.map (subst_term env) terms)

(* rename variables in a literal *)

    let shuffle env (pred, terms) =
      let rec loop env terms =
	match terms with
	  [] -> env
	| Val value :: terms -> loop env terms
	| Var var :: terms ->
	    match lookup env var with
	      None -> loop (extend env var (fresh())) terms
	    | Some _ -> loop env terms in
      loop env terms

    let rename_literal literal =
      subst_literal (shuffle [] literal) literal

(* unification *)

    let rec chase env term =
      match term with
	Var var ->
	  (match lookup env var with
	    None -> Var var
	  | Some term -> chase env term)
      | term -> term

    let unify_term env term term' =
      let term = chase env term in
      let term' = chase env term' in
      if eqt term term' then
	Some env
      else
	match term with
	  Var var -> Some (extend env var term')
	| _ ->
	    match term' with
	      Var var -> Some (extend env var term)
	    | _ -> None

    let rec unify_terms env terms terms' =
      match terms, terms' with
	[], [] -> Some env
      | term::terms, term'::terms' ->
	  (match unify_term env term term' with
	    None -> None
	  | Some env -> unify_terms env terms terms')
      | _ -> None

    let unify (pred, terms) (pred', terms') =
      if not (eqp pred pred') then
	None
      else
	unify_terms [] terms terms'

(* clauses *)

    type clause = literal * literal list

    let mkclause head body = head, body

    let gethead (head, body) = head

    let getbody (head, body) = body

(* A clause is safe if every variable in the head is also in the body. *)

    let rec safe_var var body =
      match body with
	[] -> false
      | (pred, terms) :: body ->
	  List.mem (Var var) terms || safe_var var body

    let safe_term term body =
      match term with
	Var var -> safe_var var body
      | _ -> true

    let safe ((pred, terms), body) =
      let rec loop terms =
	match terms with
	  [] -> true
	| term :: terms ->
	    safe_term term body && loop terms in
      loop terms

(* rename variables in a clause *)

    let subst_clause env (literal, literals) =
      (subst_literal env literal, List.map (subst_literal env) literals)

    let rename_clause (literal, literals) =
      let env = List.fold_left shuffle (shuffle [] literal) literals in
      subst_clause env (literal, literals)

(* primitives *)

    type primitive = int -> value list -> value list option

    let prims = Hashtbl.create 7

    let add_primitive symbol in_arity prim =
      let symbol = intern symbol in
      if in_arity < 0 then
	failwith "bad arity in add_primitive"
      else
	Hashtbl.replace prims symbol (symbol, in_arity, prim)

(* theory *)

(* A theory is implemented as a hash table. *)
    type theory = (string, clause list) Hashtbl.t

    let create = Hashtbl.create

    let copy = Hashtbl.copy

    let literal_key (pred, terms) =
      pred ^ "/" ^ string_of_int (List.length terms)

    let clause_key (literal, _) =
      literal_key literal

    let get_with_key tbl key =
      try Hashtbl.find tbl key with Not_found -> []

    let get tbl literal =
      get_with_key tbl (literal_key literal)

    exception Unsafe_clause

    let assume tbl clause =
      if not (safe clause) then
	raise Unsafe_clause;
      let key = clause_key clause in
      let clauses = get_with_key tbl key in
      if not (List.mem clause clauses) then
	Hashtbl.replace tbl key (clause :: clauses)

    let retract tbl clause =
      let key = clause_key clause in
      let pred c = c <> clause in
      let clauses = List.filter pred (get_with_key tbl key) in
      match clauses with
	[] ->  Hashtbl.remove tbl key
      | _ :: _ -> Hashtbl.replace tbl key clauses

(* prover *)

(* The remaining functions in this file implement the tabled logic
   programming algorithm described in "Efficient Top-Down Computation of
   Queries under the Well-Founded Semantics", Chen, W., Swift, T., and
   Warren, D. S., J. Logic Prog. Vol. 24, No. 3, pp. 161-199.  Another
   important reference is "Tabled Evaluation with Delaying for General
   Logic Programs", Chen, W., and Warren, D. S., J. ACM, Vol. 43, No. 1,
   Jan. 1996, pp. 20-74. *)

(* A subgoal is the item that is tabled by this algorithm. *)

    type subgoal =
	{ literal: literal;		(* the subgoal *)
	  mutable facts: literal list;	(* derived facts *)
	  mutable waiters: waiter list } (* waiters of this subgoals *)
    and waiter =
	subgoal 			(* subgoal of clause waiting *)
	  * clause			(* clause awaiting result *)

(* resolve a clause with a literal *)
    let resolve (head, body) literal =
      match body with
	[] -> None
      | selected :: body ->
	  let renamed = rename_literal literal in
	  match unify selected renamed with
	    None -> None
	  | Some env ->
	      Some (subst_clause env (head, body))

    let prove theory literal =
      let subgoals = Literaltbl.create 128 in (* table of subgoals *)

      let rec fact subgoal literal =	(* handle a derived fact *)
	if not (mem_literal literal subgoal.facts) then begin
	  subgoal.facts <- literal :: subgoal.facts; (* record fact *)
	  let use_fact (sg, cs) =
	    match resolve cs literal with
	      None -> ()
	    | Some cs' -> add_clause sg cs' in (* tell waiters *)
	  List.iter use_fact subgoal.waiters (* about new fact *)
	end

      and rule subgoal clause selected = (* handle a derived rule *)
	try
	  let sg = Literaltbl.find subgoals selected in
	  sg.waiters <- (subgoal, clause) :: sg.waiters; (* add to waiters *)
	  let use_clause fact =		(* so told about new facts *)
	    match resolve clause fact with
	      None -> ()
	    | Some cs -> add_clause subgoal cs in (* tell waiters about *)
	  List.iter use_clause sg.facts (* current facts *)
	with Not_found ->
	  let sg = {
	    literal = selected;		(* create new subgoal *)
	    facts = [];
	    waiters = [subgoal, clause]; (* to prove clause *)
	  } in
	  Literaltbl.replace subgoals selected sg;
	  search sg

      and add_clause subgoal clause =
	match clause with
	  (literal, []) -> fact subgoal literal
	| (_, selected :: _)  -> rule subgoal clause selected

      and search_theory subgoal =		(* search for proofs *)
	let search_clause clause =		(* of the subgoal using *)
	  let renamed = rename_clause clause in (* relevant assumptions *)
	  let selected, _ = renamed in	(* from the theory *)
	  let env = unify subgoal.literal selected in
	  match env with
	    None -> ()
	  | Some env ->
	      add_clause subgoal (subst_clause env renamed) in
	List.iter search_clause (get theory subgoal.literal)

      and equal_primitive subgoal a b =	(* the equality predicate *)
	let equal_test a b =
	  match a, b with		(* the equal tests *)
	    Val x, Val y -> 		(* passes when both *)
	      if D.equal x y then	(* arguments are the *)
		fact subgoal (same, [a; b]) (* same constant *)
	  | _ -> () in
	match unify_term [] a b with	(* unify the arguments *)
	  None -> equal_test a b	(* and substitute the *)
	| Some env -> 			(* resulting environment *)
	    equal_test (subst_term env a) (subst_term env b)

      and apply_prim subgoal symbol in_arity out_arity prim l =
	let rec tag_values acc values =	(* found a fact *)
	  match values with		(* reverse list and *)
	    [] ->			(* tag values *)
	      fact subgoal (symbol, acc)
	  | v :: values ->
	      tag_values (Val v :: acc) values in
	let rec unify_results acc args results =
          let results = List.map (fun v -> Val v) results in
	  match unify_terms [] args results with
              None -> ()
            | Some _ -> tag_values results acc in
	let result acc args results =
	  match results with
	    None -> ()			(* Predicate failed *)
	  | Some results ->
	      unify_results acc args results in
	let rec loop acc in_arity args =
	  if in_arity <= 0 then
	    result acc args (prim out_arity (List.rev acc))
	  else
	    match args with 		(* extract values from args *)
	      Val v :: args ->
		loop (v :: acc) (in_arity - 1) args
	    | _ -> () in	   (* fail when variable is an arg *)
	loop [] in_arity l

      and search subgoal =		(* search for proofs *)
	match subgoal.literal with	(* of the subgoal by *)
	  pred, [a; b] when eqp pred same -> (* evaluating primitives *)
	    equal_primitive subgoal a b
	| pred, l ->
	    try
	      let symbol, in_arity, prim = Hashtbl.find prims pred in
	      let arity = List.length l in
	      if arity >= in_arity then
		let out_arity = arity - in_arity in (* use prim *)
		apply_prim subgoal symbol in_arity out_arity prim l
	    with Not_found ->
	      search_theory subgoal in	(* otherwise use theory *)

      let subgoal = {			(* initiate a proof *)
	literal = literal;		(* by creating a subgoal *)
	facts = [];			(* with no waiters *)
	waiters = [];
      } in
      Literaltbl.replace subgoals literal subgoal;
      search subgoal;			(* search for proofs and *)
      subgoal.facts		        (* then return derived facts *)
  end

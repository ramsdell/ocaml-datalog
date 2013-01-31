/* A parser for Datalog programs. */

/* This parser assumes values in terms are strings. */
%token <string> VAL VAR
%token LPAREN, RPAREN, EQUAL, COMMA, IMPLY, PERIOD, QUESTION, EOF

%start program a_clause

%type <Prover.clause list * Prover.literal> program

%type <Prover.clause> a_clause

%%

/* A program is a sequence of clauses followed by a query. */
program:
  clauses query EOF { List.rev $1, $2 }
;

/* Parse and return just one clause. */

a_clause:
  clause PERIOD EOF              { $1 }
;

clauses:
                                 { [] }
| clauses clause PERIOD          { $2 :: $1 }
;

/* A Horn clause. */
clause:
  atom                           { Prover.mkclause $1 [] }
| atom IMPLY atoms               { Prover.mkclause $1 (List.rev $3) }
;

query:
  atom QUESTION                  { $1 }
;

atoms:
  atom                           { [$1] }
| atoms COMMA atom               { $3 :: $1 }
;

atom:
  VAL                            { Prover.mkliteral $1 [] }
| VAL LPAREN terms RPAREN        { Prover.mkliteral $1 (List.rev $3) }
| term EQUAL term                { Prover.mkliteral "=" [$1; $3] }
;

terms:
  term                           { [$1] }
| terms COMMA term               { $3 :: $1 }
;

term:
  VAR                            { Prover.mkvar $1 }
| VAL                            { Prover.mkval $1 }
;

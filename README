ocaml-datalog
=============

This package contains a lightweight deductive database system written
in OCaml.  Queries and database updates are expressed using Datalog--a
declarative logic language in which each formula is a function-free
Horn clause, and every variable in the goal of a clause must appear in
the body of the clause.  The use of Datalog syntax and an
implementation based on tabling intermediate results, ensures that all
queries terminate.  This is an OCaml version of the Lua implementation
at <http://datalog.sf.net>, and documentation on that site is
relevant.

This Datalog engine was extracted from the runtime system of the
Cryptographic Protocol Programming Language (CPPL) at
<http://www.ccs.neu.edu/home/ramsdell/tools/cppl-1.6.tar.gz>.  The
Cryptographic Protocol Programming Language (CPPL) facilitates the
design and implementation of hand-crafted protocols for electronic
commerce and cross-organization distributed applications.  The Datalog
engine is used to establish trust relations between communicating
participants.

To build the software, type "make".

$ ./datalog --help
Usage: datalog  [-o output] [input]
  -o name - send output to this file (default stdout)
  --      - treat remaining args as file names, where - means stdin
  -help  Display this list of options
  --help  Display this list of options
$

To run the test suite, type "./try".  No output means all tests passed.

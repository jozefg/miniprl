(* The underlying computation system used as the basis for *what we're
 * refining. The binding structure is just De Bruijn so I've provided
 * a few functions for handling substitution, abstraction, all that jazz.
 *)
signature TERM =
sig
  datatype t
    = VAR of int (* A De Bruijn variable *)
    | LAM of t (* BINDS. Equivalent to fn x => e *)
    | AP of t * t (* Apply a function to an argument *)
    | PI  of t * t (* BINDS. The type of functions, Think Pi x : A. B *)

    | PAIR of t * t (* A tuple, (e, e) *)
    | FST of t (* Grab the first component of a tuple, think #1 e *)
    | SND of t (* Grab the second component of a tuple, think #2 e *)
    | SIG of t * t (* BINDS. The type for pairs tuples with. Think Sigma x : A. B *)

    | ZERO (* Just a constant for numeric zero *)
    | SUCC of t (* n + 1 of a number *)
    | REC of t * t * t (* BINDS. Primitive recursion for natural numbers *)
    | NAT (* The type of natural numbers *)

    | TT (* The "term" level unit, () *)
    | UNIT (* The corresponding type level *)

    | EQ of t * t * t (* The proposition for equality *)
    | CEQ of t * t (* The stricter, computational version of equality *)
    | BASE (* The type of all terms *)
    | UNI of int (* A universe for all types smaller than some i *)
    | PER of t (* BINDS. A type for lifting a PER to behave as a type *)
    | FIX of t (* BINDS. Purely for convenience, a fixpoint operator *)
    | CUST of (Guid.t * t list) (* A custom operator supplied by a user *)

  (* Given a term, an index, and a second term, replace all
   * occurences of the index in the second term with the first
   * term
   *)
  val subst : t -> int -> t -> t

  (* Increase free variables in a given term by N2
   * but leave all free variables below N1 alone
   *)
  val lift  : int -> int -> t -> t

  (* Decrease free variables in a given term by N2
   * but leave all free variables below N1 alone
   *)
  val lower : int -> int -> t -> t

  (* Return a list of all the free variables in a term,
   * the list shall contain no duplicated entries.
   *)
  val freevars : t -> int list
end

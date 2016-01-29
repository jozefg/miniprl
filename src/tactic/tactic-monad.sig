(* A tactic needs to make use of some nondeterminism. We need
 * to be able to express this notion of "try to do this for a while,
 * or give up and do this". Rather than using exceptions ala sml-lcf,
 * we've opted to make it explicit with something like a continuation
 * monad.
 *
 * The main reason for this is because I want it to be extremely explicit
 * where backtracking might happen and this will force us to handle the
 * tactic failures explicitly. It will also make this easier to translate
 * to Haskell if someone was so inclined :)
 *)
signature TACTIC_MONAD =
sig
  type ('result, 'a) t

  val map    : ('a -> 'b) -> ('result, 'a) t -> ('result, 'b) t
  val <>     : ('result, 'a -> 'b) t * ('result, 'a) t -> ('result, 'b) t
  val return : 'a -> ('result, 'a) t
  val >>=    : ('result, 'a) t * ('a -> ('result, 'b) t) -> ('result, 'b) t

  val sequence : ('result, 'a) t list -> ('result, 'a list) t

  val run    : ('a, 'a) t -> 'a option
  val choose : (unit -> ('result, 'a) t) list -> ('result, 'a) t
  val fail   : ('result, 'a) t
end

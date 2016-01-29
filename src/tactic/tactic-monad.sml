(* A little bit of tricksy code, it helps to recall what CPS is *)
structure TacticMonad :> TACTIC_MONAD =
struct
  (* We actually implement the monad using CPS.
   * We pervasively plumb that option through the result type
   * because it makes it easy to implement fail.
   *)
  type ('result, 'a) t = ('a -> 'result option) -> 'result option

  fun map f a k = a (k o f)
  fun op<> (f, a) k = f (fn fv =>
                      a (fn av => k (fv av)))

  fun return a k = k a
  fun >>= (a, f) k = a (fn av => f av k)

  infixr >>=
  fun sequence xs =
    case xs of
        [] => return []
      | mx :: mxs =>
        mx >>= (fn x => map (fn xs => x :: xs) (sequence mxs))

  fun run a = a SOME

  fun fail k = NONE
  fun choose [] = fail
    | choose (c :: cs) = fn k =>
      case c () k of
          SOME a => SOME a
        | NONE => choose cs k
end

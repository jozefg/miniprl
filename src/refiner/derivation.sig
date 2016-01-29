(* This is the fundamental piece of evidence in the refinement logic.
 * We have one way of forming a derivation for each rule in our PRL
 * and we'll create derivations each time we construct a proof in our logic.
 * More over, each derivation can be "extracted" into a computational
 * realizer.
 *
 * Unforunately, derivations all have a binding structure which means more
 * De Bruijn indices.
 *)
signature DERIVATION =
sig
  (* I've annotated each derivation with the rule I intend it
   * to represent for motivation, this will be mechanized later
   * in the refiner by a tactic in rules/.
   *
   * The peculiar notation of H >> ... should be understood
   * as \Gamma \vdash in many ways but looks significantly better
   * in ASCII :)
   *)
  datatype t
      (* H >> Pi x : A. B = Pi x : A. B in U(i)
       *   H >> A = A in U(i)
       *   H, x : A >> B(x) = B(x) in U(i)
       *)
    = PI_TYPE of t * t (* BINDS *)
end

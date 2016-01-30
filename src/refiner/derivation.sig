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
   *
   * IMPORTANT: Derivations in here contain terms (for things like application)
   * we silently allow to point to binding sites. Note that most occurences
   * of ints specify universe levels (i in U(i))
   *)
  datatype t
      (* H >> Pi x : A. B = Pi x : A. B in U(i)
       *   H >> A = A in U(i)
       *   H, x : A >> B(x) = B(x) in U(i)
       *)
    = PI_EQ of int * t * t (* BINDS *)
    (* H >> Pi x : A. B
     *   H >> A = A in U(i)
     *   H, x : A >> B
     *)
    | PI_INTRO of int * t * t (* BINDS *)
    (* H >> C
     *   H(i) = Pi x : A. B
     *   H >> a = a in A
     *   H, [a/x]B >> C
     *)
    | PI_ELIM of int * Term.t * t * t (* BINDS *)
    (* H >> \x. M = \y. N in Pi x : A. B
     *   H >> A = A in U(i)
     *   H, x : A >> M = N in B
     *)
    | LAM_EQ of int * t * t (* BINDS *)
    (* H >> M N = M' N' in B
     *   H >> M = M' in Pi x : A. B'
     *   H >> N = N' in A
     *   H >> [N/x]B' = B in U(i)
     *)
    | AP_EQ of int * Term.t * t * t (* We require the function type to be provided *)
    (* H >> M = N in Pi x : A. B
     *   H >> M = M in Pi x : A. B
     *   H >> N = N in Pi x : A. B
     *   H, x : A >> M x = N x in B
     *)
    | FUN_EXT of t * t * t (* BINDS *)
end

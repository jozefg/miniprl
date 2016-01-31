(* This module is sort of the back-end of the the proof assistant. After
 * we've constructed a derivation using the rule-tactics, we'll feed the
 * result into this thing. This will compute the underlying program that we
 * can actually run. If we've proven A, it computes a program so that a : A.
 *)
signature EXTRACT =
sig
  (* Given a derivation, this computes the realizer in the underlying
   * computation system for it. All the irrelevant details of the proof
   * (like how we established an equality) will be dropped and all that
   * remains is some program we can run.
   *
   * The supplied derivation is required to be well-formed obviously.
   *)
  val extract : Derivation.t -> Term.t
end

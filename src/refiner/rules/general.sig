(* These are just the random left over rules that aren't specific
 * to any types
 *)
signature GENERAL_RULES =
sig
  (* H >> C
   *   H >> t = t in C
   * Uses: WITNESS
   *)
  val Witness : Term.t -> PrlTactic.t

  (* H >> C
   *   opid is a lemma proving L
   *   H, L >> C
   * Uses: CUT
   *)
  val Cut : RefinerConfig.t -> Guid.t -> PrlTactic.t

  (* H >> C
   *   H(i) = C
   * Uses: VAR
   *)
  val Hyp : Utils.target -> PrlTactic.t

  (* There isn't a nice rule for this really. This rule
   * finds every occurence of the Guid given and expands
   * it according to what the refiner config says is its
   * extract.
   *)
  val Unfold : RefinerConfig.t -> Guid.t -> PrlTactic.t
end

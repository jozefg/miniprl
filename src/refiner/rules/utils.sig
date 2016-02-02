signature UTILS =
sig
  type universe = int
  type target = int

  exception MalformedEvidence

  (* This is a strange form of Term.subst useful in the refiner
   * but no where else. It substitutes into the term normally,
   * but doesn't decrement all of the free variables higher than
   * what we've substituted for so the term still only makes
   * sense in the original context. Eg
   * substOpen M 0 (var(0) + var(1)) = M + var(1) and not
   * M + var(0) as would be normal.
   *)
  val substOpen : Term.t -> int -> Term.t -> Term.t

  (* This checks whether a given term is proof relevant or not.
   * This is important because we're only allowed to use hidden
   * hypothesis to prove irrelevant goals. This is necessarily
   * conservative, deciding whether a given program is irrelevant
   * is undecidable in general and so we just return false if the
   * argument isn't WHNF.
   *)
  val irrelevant : Term.t -> bool

  (* Since the common case is to be adding visible hypothesis to
   * the context, it pays to have an extra operator to do so.
   * ::: behaves exactly like :: but tags the hypothesis as visible
   *)
  val ::: : Term.t * Goal.context -> Goal.context
end

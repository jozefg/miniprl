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
end

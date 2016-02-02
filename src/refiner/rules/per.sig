signature PER_RULES =
sig

  (* H >> per(x.y.A) = per(x.y.A') in U(i)
   *   H, x : base, y : base >> A = A in U(i)
   *   H, x : base, y : base >> A' = A' in U(i)
   *   H, x : base, y : base, z : A >> A'
   *   H, x : base, y : base, z : A' >> A
   *   H, a : base, b : base, z : [a, b/x, y]A >> [b, a/x, y]A
   *   H, a : base, b : base, c : base, z1 : [a, b/x, y]A, z2 : [b, c/x, y]A
   *      >> [a, c/x, y]A
   * Uses: PER_EQ
   * Note: Easily the most complicated rule in the system.
   *)
  val Eq : PrlTactic.t

  (* H >> a = b in per(x.y.A)
   *   H >> a in base
   *   H >> b in base
   *   H >> [a, b/x, y]A
   *   H >> per(x.y.A) = per(x.y.A) in U(i)
   * Uses: PER_MEM_EQ
   *)
  val MemEq : Utils.universe -> PrlTactic.t

  (* H >> C
   *   H(i) = a = b in per(x.y.A)
   *   H, HIDE([a, b/x, y]A) >> C
   * Uses: PER_ELIM_EQ
   * Note: This rule is actually pretty interesting, it's the only rule in the
   * whole system that introduces a hidden hypothesis. The reason is
   * that while we know that something is in a per-type only if it satisfies
   * the relation, we don't have access to evidence for this and all of
   * that will be erased when we go into a realizer. Therefore, it's important
   * that we ensure that everything which might make use of this hypothesis
   * is irrelevant. Then when it comes time to extract, we're guarenteed that
   * it's OK to substitute in TT instead of some meaningful proof.
   *)
  val ElimEq : Utils.target -> PrlTactic.t
end

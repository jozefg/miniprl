signature EQ_RULES =
sig
  (* H >> (a = b in A) = (a' = b' in A') in U(i)
   *   H >> A = A' in U(i)
   *   H >> a = a' in A
   *   H >> b = b' in A'
   * Uses: EQ_EQ
   *)
  val Eq : PrlTactic.t

  (* H >> tt = tt in (a = b in A)
   *   H >> a = b in A
   * Uses: EQ_MEM_EQ
   *)
  val MemEq : PrlTactic.t

  (* H >> a = b in A
   *   H >> b = a in A
   * Uses: EQ_SYM
   *)
  val Sym : PrlTactic.t

  (* H >> [a/x]C
   *   H, x : A >> C in U(i)
   *   H >> a = b in A
   *   H >> [b/x]C
   * Uses: EQ_SUBST
   * Note that first supplied term should be a = b in A and
   * the second one should be C.
   *)
  val Subst : Utils.universe -> Term.t -> Term.t -> PrlTactic.t
end

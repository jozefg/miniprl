signature CEQ_RULES =
sig
  (* H >> (a ~ b) = (a' ~ b') in U(i)
   *   H >> a ~ a'
   *   H >> b ~ b'
   * Uses: CEQ_EQ
   *)
  val Eq : PrlTactic.t

  (* H >> tt = tt in (a ~ b)
   *   H >> a ~ b
   * Uses: CEQ_MEM_EQ
   *)
  val MemEq : PrlTactic.t
  (* H >> a ~ b
   *   H >> b ~ a
   * Uses: CEQ_SYM
   *)
  val Sym : PrlTactic.t

  (* H >> [a/x]C
   *   H >> a ~ b
   *   H >> [b/x]C
   * Uses: CEQ_SUBST
   * Note: the first tactic should be a ~ b and the second should C
   *)
  val Subst : Term.t -> Term.t -> PrlTactic.t

  (* H >> a ~ b
   *   a |-> a'
   *   H >> a' ~ b
   * Uses: CEQ_STEP
   *)
  val Step : PrlTactic.t

  (* H >> a ~ a
   *  Uses: CEQ_REFL
   *)
  val Refl : PrlTactic.t
end

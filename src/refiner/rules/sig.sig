signature SIG_RULES =
sig
  (* H >> Sig x : A. B = Sig x : A'. B' in U(i)
   *   H >> A = A' in U(i)
   *   H, x : A >> B = B' in U(i)
   * Uses: SIG_EQ
   *)
  val Eq : PrlTactic.t

  (* H >> Sig x : A. B
   *   H >> a = a in A
   *   H >> [a/x]B
   *   H, a : A >> B = B in U(i)
   * Uses: SIG_INTRO
   *)
  val Intro : Utils.universe -> Term.t -> PrlTactic.t

  (* H >> C
   *   H(i) = Sig x : A. B
   *   H, x : A, y : B >> C
   * Uses: SIG_ELIM
   *)
  val Elim : Utils.target -> PrlTactic.t

  (* H >> pair(a; b) = pair(a'; b') in Sig x : A. B
   *  H >> a = a' in A
   *  H >> b = b' in [a/x]B
   *  H, x : A >> B = B in U(i)
   * Uses: PAIR_EQ
   *)
  val PairEq : Utils.universe -> PrlTactic.t

  (* H >> fst(a) = fst(a') in A
   *   H >> a = a' in Sig x : A. B
   * Uses: FST_EQ
   * Note that the supplied term should be Sig x : A . B
   *)
  val FstEq : Term.t -> PrlTactic.t

  (* H >> snd(a) = snd(a') in B'
   *   H >> a = a' in Sig x : A. B
   *   H >> [a/x]B = B' in U(i)
   * Uses: SND_EQ
   * Note that the supplied term should be Sig x : A . B
   *)
  val SndEq : Utils.universe -> Term.t -> PrlTactic.t
end

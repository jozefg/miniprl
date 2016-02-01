signature BASE_RULES =
sig
  (* H >> base = base in U(i)
   *)
  val Eq : PrlTactic.t

  (* H >> a = b in base
   *   x in FV(a) => H(x) = base
   *   x in FV(b) => H(x) = base
   *   H >> a ~ b
   *)
  val MemEq : PrlTactic.t

  (* H >> C
   *   H(i) = (a = b in base)
   * H, a ~ b >> C
   *)
  val ElimEq : Utils.target -> PrlTactic.t
end

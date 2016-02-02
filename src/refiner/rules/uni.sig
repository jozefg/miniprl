signature UNI_RULES =
sig
  (* H >> U(i) = U(i) in U(i + 1)
   * Uses: UNI_EQ
   *)
  val Eq : PrlTactic.t

  (* H >> A = B in U(i + 1)
   *   H >> A = B in U(i)
   * Uses: CUMULATIVE
   *)
  val Cumulative : PrlTactic.t
end

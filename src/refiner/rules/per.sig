signature PER_RULES =
sig
  val Eq : PrlTactic.t
  val MemEq : PrlTactic.t
  val ElimEq : Utils.target -> PrlTactic.t
end

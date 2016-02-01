signature NAT_RULES =
sig
  val Eq : PrlTactic.t
  val Intro : PrlTactic.t
  val Elim : Utils.target -> PrlTactic.t
  val ZeroEq : PrlTactic.t
  val SuccEQ : PrlTactic.t
  val RecEQ : PrlTactic.t
end

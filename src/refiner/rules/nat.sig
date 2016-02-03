signature NAT_RULES =
sig
  val Eq : PrlTactic.t
  val Intro : PrlTactic.t
  val Elim : Utils.target -> PrlTactic.t
  val ZeroEq : PrlTactic.t
  val SuccEq : PrlTactic.t
  val RecEq : PrlTactic.t
end

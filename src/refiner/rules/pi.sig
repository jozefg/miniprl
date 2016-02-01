signature PI_RULES =
sig
  val Eq     : PrlTactic.t
  val Intro  : Utils.universe -> PrlTactic.t
  val Elim   : Utils.target -> Term.t -> PrlTactic.t
  val LamEq  : Utils.universe -> PrlTactic.t
  val ApEq   : Utils.universe -> Term.t -> PrlTactic.t
  val FunExt : PrlTactic.t
end

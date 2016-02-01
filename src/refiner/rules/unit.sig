signature UNIT_RULES =
sig
  (* H >> unit = unit in U(i)
   * Uses: UNIT_EQ
   *)
  val Eq : PrlTactic.t

  (* H >> unit
   * Uses: UNIT_INTRO
   *)
  val Intro : PrlTactic.t

  (* H >> tt = tt in unit
   * Uses: TT_EQ
   *)
  val TTEq : PrlTactic.t

end

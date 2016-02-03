structure CompileScript :> COMPILE_SCRIPT =
struct
  structure Prim = PrimitiveTactics(PrlTactic)
  open Rules Script

  val choose = Prim.choose
  infixr choose

  (* To keep the tactics readable, many of the uber tactics raise
   * this and then catch it and convert it into Prim.fail. It's raised
   * when the supplied data wasn't provided for something critical to the
   * tactic.
   *)
  exception WrongData

  (* Correspondingly, we have a helper function which raises wrong data
   * for the places where we need some option to be present
   *)
  fun get (SOME a) = a
    | get NONE = raise WrongData

  (* Implementing the uber tactics is kind of fun, we just combine all the
   * possible relevant tactics with choose and let the backtracking engine
   * sort out which is the right one.
   *)
  fun Intro {term, uni} =
    (Nat.Intro choose
     Sig.Intro (Option.getOpt (uni, 0)) (get term) choose
     Unit.Intro choose
     Pi.Intro (Option.getOpt (uni, 0)))
    handle WrongData => Prim.fail

  fun Elim {target, term} =
    (Base.ElimEq target choose
     Per.ElimEq target choose
     Nat.Elim target choose
     Pi.Elim target (get term) choose
     Sig.Elim target)
    handle WrongData => Prim.fail

  fun Eq {term, uni} =
    (Base.Eq choose
     Base.MemEq choose
     Ceq.Eq choose
     Ceq.MemEq choose
     Eq.Eq choose
     Eq.MemEq choose
     General.HypEq choose
     Nat.Eq choose
     Nat.ZeroEq choose
     Nat.SuccEq choose
     Per.Eq choose
     Per.MemEq (Option.getOpt (uni, 0)) choose
     Pi.Eq choose
     Pi.LamEq (Option.getOpt (uni, 0)) choose
     Pi.ApEq (Option.getOpt (uni, 0)) (get term) choose
     Sig.Eq choose
     Sig.PairEq (Option.getOpt (uni, 0)) choose
     Sig.FstEq (get term) choose
     Sig.SndEq (Option.getOpt (uni, 0)) (get term) choose
     Uni.Eq choose
     Unit.Eq choose
     Unit.TTEq)
    handle WrongData => Prim.fail

  fun compile script =
    case script of
        SEQUENCE ts =>
        List.foldl (fn (SINGLE t, rest) => Prim.next (rest, compile t)
                     | (MULTI ts, rest) => Prim.split (rest, List.map compile ts))
                   Prim.id
                   ts
      | SPLIT (t, ts) => Prim.split (compile t, List.map compile ts)
      | CHOOSE ts => List.foldl Prim.choose Prim.fail (List.map compile ts)
      | ID => Prim.id
      | FAIL => Prim.fail
      | INTRO data => Intro data
      | ELIM data => Elim data
      | EQ data => Eq data
end

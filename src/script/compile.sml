structure CompileScript :> COMPILE_SCRIPT =
struct
  structure Prim = PrimitiveTactics(PrlTactic)
  open Rules Script TacticMonad

  infix >>=

  val choose = Prim.choose
  infixr choose

  (* We have a helper function which fails out if the data isn't there
   * for the places where we need some option to be present
   *)
  fun get f (SOME a) = f a
    | get f NONE = Prim.fail

  (* Implementing the uber tactics is kind of fun, we just combine all the
   * possible relevant tactics with choose and let the backtracking engine
   * sort out which is the right one.
   *)
  fun Intro {term, uni} =
    (Nat.Intro choose
     get (Sig.Intro (Option.getOpt (uni, 0))) term choose
     Unit.Intro choose
     Pi.Intro (Option.getOpt (uni, 0)))
    handle WrongData => Prim.fail

  fun Elim {target, term} =
    (Base.ElimEq target choose
     Per.ElimEq target choose
     Nat.Elim target choose
     get (Pi.Elim target) term choose
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
     Nat.RecEq choose
     Per.Eq choose
     Per.MemEq (Option.getOpt (uni, 0)) choose
     Pi.Eq choose
     Pi.LamEq (Option.getOpt (uni, 0)) choose
     get (Pi.ApEq (Option.getOpt (uni, 0))) term choose
     Sig.Eq choose
     Sig.PairEq (Option.getOpt (uni, 0)) choose
     get Sig.FstEq term choose
     get (Sig.SndEq (Option.getOpt (uni, 0))) term choose
     Uni.Eq choose
     Unit.Eq choose
     Unit.TTEq)
    handle WrongData => Prim.fail

  fun compile (c : RefinerConfig.t) script =
    case script of
        SEQUENCE ts =>
        List.foldl (fn (SINGLE t, rest) => Prim.next (rest, compile c t)
                     | (MULTI ts, rest) =>
                       Prim.split (rest, List.map (compile c) ts))
                   Prim.id
                   ts
      | SPLIT (t, ts) => Prim.split (compile c t, List.map (compile c) ts)
      | CHOOSE ts => List.foldl Prim.choose Prim.fail (List.map (compile c) ts)
      | REPEAT t => Prim.repeat (compile c t)
      | ID => Prim.id
      | FAIL => Prim.fail
      | INTRO data => Intro data
      | ELIM data => Elim data
      | EQ data => Eq data
      | FUNEXT => Pi.FunExt
      | CUMULATIVE => Uni.Cumulative
      | ASSUMPTION i => General.Hyp i
      | CUT id => General.Cut c id
      | EXPAND id => General.Unfold c id
      | WITNESS t => General.Witness t
      | SYM => Eq.Sym choose Ceq.Sym
      | STEP => Ceq.Step
      | REFL => Ceq.Refl
      | SUBST {uni, pattern, equality} =>
        Eq.Subst (Option.getOpt (uni, 0)) equality pattern choose
        Ceq.Subst equality pattern

end

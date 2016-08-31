(* The first line of tests we have for exercising the refiner is to
 * run a bunch of very simple proofs through it. If anything fails to
 * be discharged by this process we immediately bomb out. This is a little
 * on the crude side but very effective for detecting regressions
 *)
structure SimpleProofs =
struct
  exception SimpleProofsFailed of string

  fun mustProve msg a b =
    case Refiner.prove a b of
        Refiner.PROVED _ => ()
      | Refiner.INCOMPLETE _ => raise SimpleProofsFailed (msg ^ ": got incomplete!")
      | Refiner.FAILED => raise SimpleProofsFailed (msg ^ ": got fail!")
  fun mustFail msg a b =
      case Refiner.prove a b of
          Refiner.FAILED => ()
        | Refiner.INCOMPLETE _ => raise SimpleProofsFailed (msg ^ ": got incomplete!")
        | Refiner.PROVED _ => raise SimpleProofsFailed (msg ^ ": got passed!")

  structure P = PrimitiveTactics(PrlTactic)
  open Term Rules P
  infix next split choose

  (* Base tests *)
  val () = mustProve "Base.Eq" (EQ (BASE, BASE, UNI 0)) Base.Eq
  val () = mustProve "Base.Eq" (EQ (BASE, BASE, UNI 1)) Base.Eq
  val () = mustProve "Base.MemEq" (EQ (TT, TT, BASE)) (Base.MemEq next Ceq.Refl)
  val () = mustProve "Base.MemEq"
    (EQ (AP (LAM (VAR 0), TT), TT, BASE))
    (Base.MemEq next Ceq.Step next Ceq.Refl)
  val () = mustProve "Base.ElimEq"
    (PI (EQ (TT, UNIT, BASE), CEQ (TT, UNIT)))
    (Pi.Intro 0 split
     [ Eq.Eq split [Base.Eq, Base.MemEq next Ceq.Refl, Base.MemEq next Ceq.Refl]
     , Base.ElimEq 0 next General.Hyp 0
    ])

  (* Ceq tests *)
  val () = mustProve "Ceq.Refl" (CEQ (VAR 0, VAR 0)) Ceq.Refl
  val () = mustProve "Ceq.Refl" (CEQ (TT, TT)) Ceq.Refl
  val () = mustProve "Ceq.Refl"
    (CEQ (PI (PI (VAR 0, VAR 1), UNIT), PI (PI (VAR 0, VAR 1), UNIT)))
    Ceq.Refl
  val () = mustFail "Ceq.Refl" (CEQ (TT, UNIT)) Ceq.Refl
  val () = mustProve "Ceq.MemEq"
    (EQ (TT, TT, CEQ (UNIT, UNIT)))
    (Ceq.MemEq next Ceq.Refl)
  val () = mustProve "Ceq.MemEq"
    (EQ (TT, TT, CEQ (AP (LAM (VAR 0), UNIT), UNIT)))
    (Ceq.MemEq next Ceq.Step next Ceq.Refl)
  val () = mustProve "Ceq.Sym"
    (CEQ (UNIT, AP (LAM (VAR 0), UNIT)))
    (Ceq.Sym next Ceq.Step next Ceq.Refl)
  val () = mustProve "Ceq.Sym"
    (CEQ (AP (LAM (VAR 0), UNIT), UNIT))
    (Ceq.Sym next Ceq.Sym next Ceq.Step next Ceq.Refl)
  val () = mustProve "Ceq.Step"
    (* This basically checks that 3 + 2 = 5 *)
    (CEQ ( REC (SUCC (SUCC (SUCC ZERO)), SUCC (SUCC ZERO), SUCC (VAR 1))
         , SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))))
    (repeat Ceq.Step next Ceq.Refl)
  val () = mustProve "Ceq.Subst"
    (PI (CEQ (ZERO, SUCC ZERO), CEQ (REC (ZERO, TT, UNIT), UNIT)))
    (Pi.Intro 0 split
      [ Ceq.Eq split [Ceq.Refl, Ceq.Refl]
      , Ceq.Subst (CEQ (ZERO, SUCC ZERO)) (CEQ (REC (VAR 0, TT, UNIT), UNIT)) split
          [General.Hyp 0, Ceq.Step next Ceq.Refl]])

  (* Eq tests *)
  val () = mustProve "Eq.Eq"
    (EQ (EQ (UNIT, UNIT, UNI 0), EQ (UNIT, UNIT, UNI 0), UNI 1))
    (Eq.Eq split [Uni.Eq, Unit.Eq, Unit.Eq])
  val () = mustProve "Eq.Eq"
    (EQ (EQ (UNIT, BASE, UNI 0), EQ (UNIT, BASE, UNI 0), UNI 1))
    (Eq.Eq split [Uni.Eq, Unit.Eq, Base.Eq])
  val () = mustProve "Eq.MemEq"
    (EQ (TT, TT, EQ (UNIT, UNIT, UNI 0)))
    (Eq.MemEq next Unit.Eq)
  val () = mustProve "Eq.EqSym"
    (EQ (TT, TT, EQ (UNIT, UNIT, UNI 0)))
    (Eq.Sym next Eq.MemEq next Unit.Eq)
  val () = mustProve "Eq.Subst"
    (PI (EQ (ZERO, SUCC ZERO, NAT), CEQ (REC (ZERO, BASE, UNIT), UNIT)))
    (Pi.Intro 0 split
      [ Eq.Eq split [Nat.Eq, Nat.ZeroEq, Nat.SuccEq next Nat.ZeroEq]
      , Eq.Subst 0 (EQ (ZERO, SUCC ZERO, NAT)) (CEQ (REC (VAR 0, BASE, UNIT), UNIT)) split
        [ Ceq.Eq split [Ceq.Refl, Ceq.Refl]
        , General.Hyp 0
        , Ceq.Step next Ceq.Refl]]);

  (* Pi tests *)
  val () = mustProve "Pi.FunExt"
                (PI (UNI 0,
                 PI (UNI 0,
                 PI (PI (VAR 1, VAR 1),
                 PI (PI (VAR 2, VAR 2),
                 PI (PI (VAR 3, EQ (AP (VAR 2, VAR 0),
                                    AP (VAR 1, VAR 0),
                                    VAR 3)),
                 EQ (VAR 2, VAR 1, PI (VAR 4, VAR 4))))))))

                (Pi.Intro 1 split [Uni.Eq,
                 Pi.Intro 1 split [Uni.Eq,
                 Pi.Intro 0 split [Pi.Eq split [General.HypEq, General.HypEq],
                 Pi.Intro 0 split [Pi.Eq split [General.HypEq, General.HypEq],
                 Pi.Intro 0 split [Pi.Eq split [General.HypEq,
                                    Eq.Eq split [General.HypEq,
                                                 Pi.ApEq 0 (PI (VAR 4, VAR 4)) next
                                                         General.HypEq,
                                                 Pi.ApEq 0 (PI (VAR 4, VAR 4)) next
                                                   General.HypEq]],
                 Pi.FunExt split [General.HypEq, General.HypEq,
                     Pi.Elim 1 (VAR 0) split [
                         General.HypEq,
                         General.Hyp 0
                     ]]]]]]]);

  (* Sig tests *)
  val () = mustProve "Sig.Elim"
              (PI (UNI 0,
               PI (PI (VAR 0, UNI 0),
               PI (SIG (VAR 1, AP (VAR 1, VAR 0)),
               VAR 2))))
              (Pi.Intro 1 split [Uni.Eq,
               Pi.Intro 1 split [Pi.Eq split [Uni.Cumulative next General.HypEq, Uni.Eq],
               Pi.Intro 0 split [Sig.Eq split [General.HypEq,
                                               Pi.ApEq 1 (PI (VAR 2, UNI 0)) split
                                                       [General.HypEq, General.HypEq, Uni.Eq]],
               Sig.Elim 0 split [General.Hyp 1]]]]);

  val () = mustProve "Sig.SndEq"
              (EQ (SND (PAIR (TT, TT)), SND (PAIR (TT, TT)), UNIT))
              (Sig.SndEq 0 (SIG (UNIT, UNIT)) split [
                    Sig.PairEq 0 split [Unit.TTEq, Unit.TTEq, Unit.Eq] ,
                    Unit.Eq
                ])
end

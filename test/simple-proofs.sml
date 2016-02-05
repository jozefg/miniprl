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
end

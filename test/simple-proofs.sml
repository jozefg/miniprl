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

  structure P = PrimitiveTactics(PrlTactic)
  open Term Rules P
  infix next split choose

  (* Base tests *)
  val () = mustProve "Base.Eq" (EQ (BASE, BASE, UNI 0)) Base.Eq
  val () = mustProve "Base.Eq" (EQ (BASE, BASE, UNI 1)) Base.Eq
  val () = mustProve "MemEq" (EQ (TT, TT, BASE)) (Base.MemEq next Ceq.Refl)
  val () = mustProve "MemEq"
    (EQ (AP (LAM (VAR 0), TT), TT, BASE))
    (Base.MemEq next Ceq.Step next Ceq.Refl)
  val () = mustProve "ElimEq"
    (PI (EQ (TT, UNIT, BASE), CEQ (TT, UNIT)))
    (Pi.Intro 0 split
     [ Eq.Eq split [Base.Eq, Base.MemEq next Ceq.Refl, Base.MemEq next Ceq.Refl]
     , Base.ElimEq 0 next General.Hyp 0
    ])
end

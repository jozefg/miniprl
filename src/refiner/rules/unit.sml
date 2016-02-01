structure UnitRules :> UNIT_RULES =
struct
  open Goal PrlTactic TacticMonad Derivation Term Utils
  infix 3 >>
  infixr >>= <>

  fun Eq (cxt >> t) =
    case t of
        EQ (UNIT, UNIT, UNI i) =>
        return { goals = []
               , evidence = fn [] => UNIT_EQ
                             | _ => raise MalformedEvidence
               }
        | _ => fail
  fun Intro (cxt >> t) =
    case t of
        UNIT =>
        return { goals = []
               , evidence = fn [] => UNIT_INTRO
                             | _ => raise MalformedEvidence
               }
        | _ => fail

  fun TTEq (cxt >> t) =
    case t of
        EQ (TT, TT, UNIT) =>
        return { goals = []
               , evidence = fn [] => TT_EQ
                             | _ => raise MalformedEvidence
               }
     | _ => fail

end

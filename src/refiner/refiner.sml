structure Refiner :> REFINER =
struct
  open Goal PrlTactic TacticMonad
  infix 3 >>
  infixr >>= <>

  datatype result
    = PROVED of { deriv : Derivation.t
                , extract : Term.t
                }
    | INCOMPLETE of Goal.t list
    | FAILED

  fun prove thm (tac : PrlTactic.t) : result =
    case run (tac ([] >> thm)) of
        SOME {goals = [], evidence} =>
        PROVED { deriv = evidence []
               , extract = Extract.extract (evidence [])
               }
      | SOME {goals, evidence} => INCOMPLETE goals
      | NONE => FAILED

end

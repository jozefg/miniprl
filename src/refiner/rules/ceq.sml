structure CeqRules :> CEQ_RULES =
struct
  open Goal PrlTactic TacticMonad Derivation Term Utils
  infix 3 >>
  infixr >>= <>
  infixr 4 :::

  fun Eq (cxt >> t) =
    case t of
        EQ (CEQ (m1, n1), CEQ (m2, n2), UNI i) =>
        return { goals = [ cxt >> CEQ (m1, m2)
                         , cxt >> CEQ (n1, n2)
                         ]
               , evidence = fn [d1, d2] => CEQ_EQ (d1, d2)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun MemEq (cxt >> t) =
    case t of
        EQ (TT, TT, CEQ (m, n)) =>
        return { goals = [ cxt >> CEQ (m, n) ]
               , evidence = fn [d] => CEQ_MEM_EQ d
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun Sym (cxt >> t) =
    case t of
        CEQ (m, n) =>
        return { goals = [ cxt >> CEQ (n, m) ]
               , evidence = fn [d] => CEQ_SYM d
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun Subst eq pat (cxt >> t) =
    case eq of
        CEQ (m, n) =>
        (* Check that our pattern actually describes the current goal *)
        if subst m 0 pat = t
        then
          return { goals = [ cxt >> eq
                           , cxt >> subst n 0 pat ]
                 , evidence = fn [d1, d2] => CEQ_SUBST (pat, d1, d2)
                               | _ => raise MalformedEvidence
                 }
        else fail (* And fail if it doesn't *)
      | _ => fail

  fun Refl (cxt >> t) =
    case t of
        CEQ (m, n) =>
        if m = n
        then
          return { goals = []
                 , evidence = fn [] => CEQ_REFL
                               | _ => raise MalformedEvidence
                 }
        else fail
      | _ => fail

  fun Step (cxt >> t) =
    case t of
        CEQ (m, n) => (
       case Interpreter.parallelStep m of
           SOME m' =>
           return { goals = [ cxt >> CEQ (m', n) ]
                  , evidence = fn [d] => CEQ_STEP d
                                | _ => raise MalformedEvidence
                  }
         | _ => fail
     )
      | _ => fail

end

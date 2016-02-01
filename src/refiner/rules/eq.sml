structure EqRules :> EQ_RULES =
struct
  open Goal PrlTactic TacticMonad Derivation Term Utils
  infix 3 >>
  infixr >>= <>

  fun Eq (cxt >> t) =
    case t of
        EQ (EQ (m1, n1, A1), EQ (m2, n2, A2), UNI i) =>
        return { goals = [ cxt >> EQ (A1, A2, UNI i)
                         , cxt >> EQ (m1, m2, A1)
                         , cxt >> EQ (n1, n2, A1)
                         ]
               , evidence = fn [d1, d2, d3] => EQ_EQ (d1, d2, d3)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun MemEq (cxt >> t) =
    case t of
        EQ (TT, TT, EQ (m, n, A)) =>
        return { goals = [ cxt >> EQ (m, n, A) ]
               , evidence = fn [d] => EQ_MEM_EQ d
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun Sym (cxt >> t) =
    case t of
        EQ (m, n, A) =>
        return { goals = [ cxt >> EQ (n, m, A) ]
               , evidence = fn [d] => EQ_SYM d
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun Subst uni eq pat (cxt >> t) =
    case eq of
        EQ (m, n, A) =>
        (* Check that our pattern actually describes the current goal *)
        if subst m 0 pat = t
        then
          return { goals = [ A :: cxt >> EQ (pat, pat, UNI uni)
                           , cxt >> eq
                           , cxt >> subst n 0 pat ]
                 , evidence = fn [d1, d2, d3] => EQ_SUBST (uni, pat, d1, d2, d3)
                               | _ => raise MalformedEvidence
                 }
        else fail (* And fail if it doesn't *)
     | _ => fail
end

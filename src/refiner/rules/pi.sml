structure PiRules :> PI_RULES =
struct
  open Goal PrlTactic TacticMonad Term Derivation Utils
  infix 3 >>
  infixr >>= <>

  fun Eq (cxt >> t) =
    case t of
        EQ (PI (A1, B1), PI (A2, B2), UNI i) =>
        return { goals = [ cxt >> EQ (A1, A2, UNI i)
                         , A1 :: cxt >> EQ (B1, B2, UNI i)
                         ]
               , evidence = fn [d1, d2] => PI_EQ (d1, d2)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun Intro uni (cxt >> t) =
    case t of
        PI (A, B) =>
        return { goals = [ cxt >> EQ (A, A, UNI uni)
                         , A :: cxt >> B
                         ]
               , evidence = fn [d1, d2] => PI_INTRO (uni, d1, d2)
                             | _ => raise MalformedEvidence
               }
        | _ => fail

  fun Elim target arg (cxt >> t) =
    case nth target cxt of
        PI (A, B) =>
        return { goals = [ cxt >> EQ (arg, arg, A)
                         , subst arg 0 B :: cxt >> t
                         ]
               , evidence = fn [d1, d2] => PI_ELIM (target, arg, d1, d2)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun LamEq uni (cxt >> t) =
    case t of
        EQ (LAM b1, LAM b2, PI (A, B)) =>
        return { goals = [ cxt >> EQ (A, A, UNI uni)
                         , A :: cxt >> EQ (b1, b2, B)
                         ]
               , evidence = fn [d1, d2] => LAM_EQ (uni, d1, d2)
                             | _ => raise MalformedEvidence
               }
      | _ => fail


  fun ApEq uni tp (cxt >> t) =
    case (tp, t) of
        (PI (A, B), EQ (AP (n1, m1), AP (n2, m2), R)) =>
        return { goals = [ cxt >> EQ (n1, n2, tp)
                         , cxt >> EQ (m1, m2, A)
                         , cxt >> EQ (subst m1 0 B, R, UNI uni)
                         ]
               , evidence = fn [d1, d2, d3] => AP_EQ (uni, tp, d1, d2, d3)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun FunExt (cxt >> t) =
    case t of
        EQ (f1, f2, PI (A, B)) =>
        return { goals = [ cxt >> EQ (f1, f1, PI (A, B))
                         , cxt >> EQ (f2, f2, PI (A, B))
                         , B :: cxt >> EQ (lift 0 1 f1, lift 0 1 f2, B)
                         ]
               , evidence = fn [d1, d2, d3] => FUN_EXT (d1, d2, d3)
                             | _ => raise MalformedEvidence
               }
      | _ => fail
end

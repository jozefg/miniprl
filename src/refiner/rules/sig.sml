(* This module implements the core rules surrounding sigmas, pairs, and
 * projections. The behaviour of all of these constructs is entirely
 * determined by this code so it should be carefully examined.
 *)
structure SigRules :> SIG_RULES =
struct
  (* See pi.sml for a detailed account of how these rules are implemented *)
  open Goal PrlTactic TacticMonad Term Derivation Utils
  infix 3 >>
  infixr >>= <>
  infixr 4 :::

  fun Eq (cxt >> t) =
    case t of
        EQ (SIG (A1, B1), SIG (A2, B2), UNI i) =>
        return { goals = [ cxt >> EQ (A1, A2, UNI i)
                         , A1 ::: cxt >> EQ (B1, B2, UNI i)
                         ]
               , evidence = fn [d1, d2] => SIG_EQ (d1, d2)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun Intro uni a (cxt >> t) =
    case t of
        SIG (A, B) =>
        return { goals = [ cxt >> EQ (a, a, A)
                         , cxt >> subst a 0 B
                         , A ::: cxt >> EQ (B, B, UNI uni)
                         ]
               , evidence = fn [d1, d2, d3] => SIG_INTRO (uni, a, d1, d2, d3)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun Elim target (cxt >> t) =
    case nth (irrelevant t) target cxt of
        SOME (SIG (A, B)) =>
        return { goals = [ B ::: A ::: cxt >> lift 0 2 t ]
               , evidence = fn [d] => SIG_ELIM (target, d)
                             | _  => raise MalformedEvidence
               }
      | _ => fail

  fun PairEq uni (cxt >> t) =
    case t of
        EQ(PAIR (m1, n1), PAIR (m2, n2), SIG (A, B)) =>
        return { goals = [ cxt >> EQ (m1, m2, A)
                         , cxt >> EQ (n1, n2, subst m1 0 B)
                         , A ::: cxt >> EQ (B, B, UNI uni)
                         ]
               , evidence = fn [d1, d2, d3] => PAIR_EQ (uni, d1, d2, d3)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun FstEq tp (cxt >> t) =
    case (tp, t) of
        (SIG (A, B), EQ (FST m1, FST m2, A')) =>
        if A = A' (* Check the supplied type against the goal *)
        then return { goals = [ cxt >> EQ (m1, m2, tp) ]
                    , evidence = fn [d] => FST_EQ (tp, d)
                                  | _ => raise MalformedEvidence
                    }
        else fail (* fail if they don't match *)
      | _ => fail

  fun SndEq uni tp (cxt >> t) =
    case (tp, t) of
        (SIG (A, B), EQ (FST m1, FST m2, A')) =>
        return { goals = [ cxt >> EQ (m1, m2, tp)
                         , cxt >> EQ (subst (FST m1) 0 B, B, UNI uni) ]
               , evidence = fn [d1, d2] => SND_EQ (uni, tp, d1, d2)
                             | _ => raise MalformedEvidence
               }
      | _ => fail
end

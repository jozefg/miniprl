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

  fun Eq (cxt >> t) =
    case t of
        EQ (SIG (A1, B1), SIG (A2, B2), UNI i) =>
        return { goals = [ cxt >> EQ (A1, A2, UNI i)
                         , A1 :: cxt >> EQ (B1, B2, UNI i)
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
                         , A :: cxt >> EQ (B, B, UNI uni)
                         ]
               , evidence = fn [d1, d2, d3] => SIG_INTRO (uni, a, d1, d2, d3)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun Elim target (cxt >> t) =
    case nth target cxt of
        SIG (A, B) =>
        return { goals = [ B :: A :: cxt >> t ]
               , evidence = fn [d] => SIG_ELIM (target, d)
                             | _  => raise MalformedEvidence
               }
      | _ => fail

  fun PairEq (cxt >> t) = raise Fail ""
  fun FstEq tp (cxt >> t) = raise Fail ""
  fun SndEq tp (cxt >> t) = raise Fail ""
end

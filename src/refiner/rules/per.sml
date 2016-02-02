structure PerRules :> PER_RULES =
struct
  open Goal PrlTactic TacticMonad Derivation Term Utils
  infix 3 >>
  infixr >>= <>
  infixr 4 :::

  (* Warning! What follows is the scariest piece of De Bruijn
   * awfulness in the entire proof assistant. The tricky bit is
   * that PER_EQ needs to manipulate two terms with two open variables
   * each and there is a good deal of lifting to work around this.
   *
   * Before reading this, be sure to check the signature to see what
   * the rule is supposed to do in normal notation and double check
   * the behaviour of substOpen and lift: both are used in full
   * generality here.
   *)
  fun Eq (cxt >> t) =
    case t of
        EQ (PER A1, PER A2, UNI i) =>
        return { goals = [ BASE ::: BASE ::: cxt >> EQ (A1, A1, UNI i)
                         , BASE ::: BASE ::: cxt >> EQ (A2, A2, UNI i)
                         , A1 ::: BASE ::: BASE ::: cxt >> lift 0 1 A2
                         , A2 ::: BASE ::: BASE ::: cxt >> lift 0 1 A1
                         , A1 ::: BASE ::: BASE ::: cxt >>
                           substOpen (VAR 1) 2
                             (substOpen (VAR 2) 1 (lift 0 1 A1))
                         , lift 2 1 (lift 0 1 A1) ::: lift 0 1 A1 :::
                           BASE ::: BASE ::: BASE ::: cxt >>
                           substOpen (VAR 4) 6
                             (substOpen (VAR 2) 5 (lift 0 5 A1))
                         ]
               , evidence = fn [d1, d2, d3, d4, d5, d6] =>
                               PER_EQ (d1, d2, d3, d4, d5, d6)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun MemEq uni (cxt >> t) =
    case t of
        EQ (a, b, PER A) =>
        return { goals = [ cxt >> EQ (a, a, BASE)
                         , cxt >> EQ (b, b, BASE)
                         , cxt >> subst a 0 (subst (lift 0 1 b) 0 A)
                         , cxt >> EQ (PER a, PER a, UNI uni)
                         ]
               , evidence = fn [d1, d2, d3, d4] =>
                               PER_MEM_EQ (uni, d1, d2, d3, d4)
                             | _ => raise MalformedEvidence
               }
        | _ => fail

  (* This rule is actually pretty interesting, it's the only rule in the
   * whole system that introduces a hidden hypothesis. The reason is
   * that while we know that something is in a per-type only if it satisfies
   * the relation, we don't have access to evidence for this and all of
   * that will be erased when we go into a realizer. Therefore, it's important
   * that we ensure that everything which might make use of this hypothesis
   * is irrelevant. Then when it comes time to extract, we're guarenteed that
   * it's OK to substitute in TT instead of some meaningful proof.
   *)
  fun ElimEq target (cxt >> t) =
    case nth (irrelevant t) target cxt of
        SOME (EQ (a, b, PER A)) =>
        return { goals = [ (HIDDEN, subst a 0 (subst (lift 0 1 b) 0 A)) ::
                           cxt >> t
                         ]
               , evidence = fn [d] => PER_ELIM_EQ (target, d)
                             | _ => raise MalformedEvidence
               }
      | _ => fail
end

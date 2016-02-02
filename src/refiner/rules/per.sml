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

  (* This also features some annoying DeBruijn math to correctly apply
   * the relation to two constants. The correct way I've found of doing this
   * is to substitute in the one bound first lifted by one. This gives you
   * a relation with one binder left and the thing you've just substituted
   * in is correctly lifted past that binder. Substituting in the second
   * one normally lowers what we've just substituted in and we end up
   * with the right result.
   *)
  fun MemEq uni (cxt >> t) =
    case t of
        EQ (a, b, PER A) =>
        return { goals = [ cxt >> EQ (a, a, BASE)
                         , cxt >> EQ (b, b, BASE)
                         , cxt >> subst b 0 (subst (lift 0 1 a) 0 A)
                         , cxt >> EQ (PER a, PER a, UNI uni)
                         ]
               , evidence = fn [d1, d2, d3, d4] =>
                               PER_MEM_EQ (uni, d1, d2, d3, d4)
                             | _ => raise MalformedEvidence
               }
        | _ => fail

  (* As noted in the signature, this is the only rule in the system
   * that uses hidden hypothesis. However, from an implementation point
   * of view the only difference to us is that we don't use ::: when adding
   * this new fact to our context, instead we manually ::'s on a tuple.
   *)
  fun ElimEq target (cxt >> t) =
    case nth (irrelevant t) target cxt of
        SOME (EQ (a, b, PER A)) =>
        return { goals = [ (HIDDEN, subst b 0 (subst (lift 0 1 a) 0 A)) ::
                           cxt >> t
                         ]
               , evidence = fn [d] => PER_ELIM_EQ (target, d)
                             | _ => raise MalformedEvidence
               }
      | _ => fail
end

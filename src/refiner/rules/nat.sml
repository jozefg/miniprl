structure NatRules :> NAT_RULES =
struct
  open Goal PrlTactic TacticMonad Derivation Term Utils
  infix 3 >>
  infixr >>= <>
  infixr 4 :::

  (* This is a nice example of a rule with no further subgoals
   * to be proven after a successful application. This is indicated
   * just by having the returned list of goals be empty. There's no
   * other distinguishing features separating "axiom" type rules
   * and so they slot uniformly into the tactic system. We still
   * check that the evidence function is correctly applied but this
   * could I suppose be elided.
   *)
  fun Eq (cxt >> t) =
    case t of
        EQ (NAT, NAT, UNI i) =>
        return { goals = []
               , evidence = fn [] => NAT_EQ
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun Intro (cxt >> t) =
    case t of
        NAT =>
        return { goals = []
               , evidence = fn [] => NAT_INTRO
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun Elim target (cxt >> t) =
    case nth (irrelevant t) target cxt of
        SOME NAT =>
        (* The De Bruijn computations are really hairy here. Here's what's
         * going on. First of all, we need to plug in specific constants for
         * every occurence of VAR target in our goal. This involves a
         * substitution but we have to use substOpen to make sure that the
         * result still makes sense in cxt.
         *
         * As an added wrinkle, we're adding things to our context so we have
         * to carefully apply lifts to the appropriate places. Finally, as
         * we change the context, the value we need to substitute for (target)
         * changes because it's at different positions in the context.
         *)
        return { goals = [ EQ (ZERO, VAR target, NAT) ::: cxt >>
                           lift 0 1 (substOpen ZERO target t)
                         , EQ (SUCC (VAR 1), VAR (target + 2), NAT) :::
                           substOpen (VAR 1) (target + 1) (lift 0 1 t) :::
                           NAT ::: cxt >>
                           substOpen (SUCC (VAR 2)) (target + 3) (lift 0 3 t)
                         ]
               , evidence = fn [d1, d2] => NAT_ELIM (target, d1, d2)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun ZeroEq (cxt >> t) =
    case t of
        EQ (ZERO, ZERO, NAT) =>
        return { goals = []
               , evidence = fn [] => ZERO_EQ
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun SuccEq (cxt >> t) =
    case t of
        EQ (SUCC m1, SUCC m2, NAT) =>
        return { goals = [ cxt >> EQ (m1, m2, NAT) ]
               , evidence = fn [d] => SUCC_EQ d
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun RecEq (cxt >> t) =
    case t of
        EQ (REC (n1, z1, s1), REC (n2, z2, s2), A) =>
        return { goals = [ cxt >> EQ (n1, n2, NAT)
                         , cxt >> EQ (z1, z2, A)
                         , lift 0 1 A ::: NAT ::: cxt >> EQ (s1, s2, lift 0 2 A)
                         ]
               , evidence = fn [d1, d2, d3] => REC_EQ (d1, d2, d3)
                             | _ => raise MalformedEvidence
               }
      | _ => fail
end

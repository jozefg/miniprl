(* This structure realizes all the rules to do with pi types.
 * Therefore, to understand/trust the behaviour of miniprl
 * it's sufficient to study this module.
 *
 * Furthermore, because there's a lot of similarities between
 * the various modules implementing the proof assistant rules
 * I've heavily documented this one explaining all the details
 * and avoided duplication by referencing it from elsewhere in
 * the program.
 *)
structure PiRules :> PI_RULES =
struct
  (* To start we toss a bunch of the standard things we'll
   * need into scope. We need goals and tactics obviously
   * because we're going to write a lot of functions of the
   * type [goal -> result choice]. Similarly, we need the
   * tactic monad in scope because we intend to use the underlying
   * monadic operations on choice that it provides.
   *
   * Term, Derivation, and Utils all provide us with the necessary
   * constructs for manipulating the content of the goals. We'll
   * be doing a lot of pattern matching on terms and every rule
   * contains a function for constructing evidence.
   *)
  open Goal PrlTactic TacticMonad Term Derivation Utils
  infix 3 >>
  infixr >>= <>
  infixr 4 :::

  (* We'll go through this tactic in more detail than the rest
   * so that it's clear how the general format of a rule goes.
   *
   * To start with, remember that a tactic actually is just a function
   * of the form goal -> result choice (where choice is our backtracking
   * monad). We start then by pattern matching on the goal. This comes
   * from the Goal module which only has one constructor so we know
   * that this tactic has the goal of proving [cxt >> t].
   *)
  fun Eq (cxt >> t) =
    (* It will often be the case that [t] isn't even the right
     * sort of goal for our tactic to be applicable. This is where
     * we need to make use of the backtracking monad, we start by
     * checking to make sure that t is even an equality between
     * Pis and if it isn't we backtrack
     *)
    case t of
        EQ (PI (A1, B1), PI (A2, B2), UNI i) =>
        (* If the tactic is of the right form we can now just
         * explain what the remaining subgoals are. The tactic doesn't
         * attempt to prove them or anything, it only states that it
         * is sufficient to prove these two goals in order to prove
         * the original one. Later on at the top level a user would
         * probably compose this rule with a dozen others to actually
         * produce a proof.
         *
         * Note that in the returned goals the first one just involves
         * showing that A1 and A2 are equal types. We leave the context
         * alone so there's now risk of messing up De Bruijn variables
         * or something. In the second goal though, we cons on A1. If
         * the user manages to prove the first subgoal we of course know
         * that A1 and A2 are indistinguishable so we could have just
         * as easily written A2 there. Even though it looks like we're
         * adding A1 to the wrong end of the context (it may refer to
         * cxt with its own free variables) we store the context so that
         * the last item is at the head of the list. Since B1 and B2 are
         * actually supposed to refer to variables of type A1 (or A2 for
         * B2) we absolutely don't lift them, this is what they're supposed
         * to do. UNI i technically should be lifted, but it has no
         * free variables so there's no point.
         *)
        return { goals = [ cxt >> EQ (A1, A2, UNI i)
                         , A1 ::: cxt >> EQ (B1, B2, UNI i)
                         ]
               (* Like all of the evidence forming functions,
                * this one is pretty trivial. It just packs the
                * supplied derivations into the appropriate derivation
                * constructor. There's no checking and we raise a
                * standard exn from Utils if anyone calls the evidence
                * function improperly. We justify the lack of checking
                * by noting that we (the refiner) are the only people
                * accessing this stuff anyways so it's redundant. Of
                * course after the fact one could easily display/validate
                * the final derivation to be extra sure nothing has gone
                * wrong.
                *)
               , evidence = fn [d1, d2] => PI_EQ (d1, d2)
                          | _ => raise MalformedEvidence
               }
      | _ => fail (* Backtracking is done using TacticMonad.fail *)

  (* The only notable difference to see in this tactic is we give
   * the rule an extra argument. It's a function from a universe level
   * (here just int) to a rule. The reason that this is necessary is
   * because we need to add A to the context for one of the subgoals.
   * However, invariants demand that we only ever add well-formed
   * types to a context. This means we need an extra subgoal to ensure
   * we maintain this invariant because it's just undecidable to check
   * typehood on our own! These subgoals are called well-formedness goals
   * (wf-goals) and they are a tremendous pain. However, in order to check
   * this one we need to know which universe A resides in which is also
   * undecidable and so we just make the user of the tactic tell us
   * which one to pick. If they choose wrong then the subgoal will be
   * unsatisfiable, but that's their problem.
   *)
  fun Intro uni (cxt >> t) =
    case t of
        PI (A, B) =>
        return { goals = [ cxt >> EQ (A, A, UNI uni)
                         , A ::: cxt >> B
                         ]
               , evidence = fn [d1, d2] => PI_INTRO (uni, d1, d2)
                             | _ => raise MalformedEvidence
               }
        | _ => fail

  (* Elim is similar to intro, it takes in some extra data to configure
   * its behaviour. In this case though it's not really for a well-formedness
   * goal, it's to tell it which hypothesis we're using from the context
   * and what term we're applying it to.
   *)
  fun Elim target arg (cxt >> t) =
    case nth (irrelevant t) target cxt of
        SOME (PI (A, B)) =>
        return { goals = [ cxt >> EQ (arg, arg, A)
                         , subst arg 0 B ::: cxt >> t
                         ]
               , evidence = fn [d1, d2] => PI_ELIM (target, arg, d1, d2)
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun LamEq uni (cxt >> t) =
    case t of
        EQ (LAM b1, LAM b2, PI (A, B)) =>
        return { goals = [ cxt >> EQ (A, A, UNI uni)
                         , A ::: cxt >> EQ (b1, b2, B)
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

  (* This rule exhibits one final stumbling block for writing rules, it
   * has some De Bruijn work. Remember that we understand that in the
   * goal cxt >> t, the free variables of t refer to specific entries
   * in cxt. The variable 0 refers to the head of the context,
   * 1 to the second item, so on and so on.
   *
   * Therefore, when we change the context we're using, we need to make
   * sure that all of the terms still make sense even in this new context.
   * All the variables need to be adjusted so that they point the right
   * spots. Sometimes we just remove a binder from a term and add something
   * to the context so no De Bruijn math needs to be done. In this case
   * though, we add something to the context and *keep using the same
   * term from the original context*. This would be a problem because
   * if our term was VAR 0, the meaning of the goal is completely
   * different!
   *
   * What we do instead is apply Term.lift to change all the free variables
   * of the terms in question so that they point to the right spots in the
   * context still. Seems simple enough, but this is a huge source of
   * bugs in practice because the errors are silent. De Bruijn variables
   * are just like pointers and they cause all sorts of pointer-like errors.
   *)
  fun FunExt (cxt >> t) =
    case t of
        EQ (f1, f2, PI (A, B)) =>
        return { goals = [ cxt >> EQ (f1, f1, PI (A, B))
                         , cxt >> EQ (f2, f2, PI (A, B))
                         (* Here is the spot I was referencing *)
                         , B ::: cxt >> EQ (lift 0 1 f1, lift 0 1 f2, B)
                         ]
               , evidence = fn [d1, d2, d3] => FUN_EXT (d1, d2, d3)
                             | _ => raise MalformedEvidence
               }
      | _ => fail
end

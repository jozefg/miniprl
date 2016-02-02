(* This is the minimum set of definitions one needs to provide in
 * order to start talking about some basic tactics. It includes
 * definitions of what are the goals and what constitutes evidence
 * for a derivation of the goals.
 *
 * Later on of course we intend to fill this in with goals = sequents
 * in our language and derivations are... derivations!
 *)
signature TACTIC =
sig
  (* A tactic is a procedure of building up evidence in a suitable
   * way. We intend to functorize this so we'll leave [derivation],
   * our evidence, abstract for now. Similarly, the things goals we
   * give tactics are also left abstract
   *
   * This abstraction pays off though because a lot of tactics can
   * be implemented independently of the type theory: reducing trusted
   * base.
   *)
  type derivation
  type goal

  (* A result is a specification of what's left to do and a way of
   * transforming derivations for the remaining goals into a final
   * derivation
   *
   * INVARIANT: [evidence] is only called with the same number of
   * derivations as there are entries in [goals]
   *)
  type result = { evidence : derivation list -> derivation
                , goals : goal list
                }

  (* This is the specialization of the nondeterminism we need,
   * our end goal is *always* a result when we run this
   *)
  type 'a choice = (result, 'a) TacticMonad.t

  (* A result is a specification of what's left to do and a way of *)
  type t = goal -> result choice
end

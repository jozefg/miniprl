(* The refiner provides various semantic tactics. These are really just little
 * ML programs for computing changes in the proof state. However, from a users
 * point of a view there should be an AST which lets a person build up semantic
 * types. This AST isn't merely limited to the primitive rules though, rather,
 * it's got several high level tactics and then several structural tactics for
 * combining them. While the semantic tactics are more fine grain, these tactics
 * are actually realistic for someone to be using to prove theorems.
 *
 * From the point of view of a frontend for miniprl, these are what tactics are:
 * a single uniform interface that's not split between various modules or even
 * areas of the implementation.
 *)
signature SCRIPT =
sig

  type intro_data = { term : Term.t option (* Used for SIG *)
                    (* Several type formers need to know at
                     * what universe they're being introduced. If
                     * nothing is supplied we default to 0. This happens
                     * for all the mega-tactics
                     *)
                    , uni  : int option
                    }
  type elim_data = { target : int
                   (* Used for the argument to PI *)
                   , term : Term.t option
                   }
  type eq_data = { term : Term.t option (* Used in fst and snd *)
                 , uni : int option
                 }
  type subst_data = { uni : int option (* This is not needed for ceq's subst *)
                    , pattern : Term.t
                    , equality : Term.t
                    }
  datatype t
    (* Given a list of tactics, we can run them end to end to end. If one
     * tactic generates multiple subgoals this will run the remaining tactics
     * on each subgoal separately
     *)
    = SEQUENCE of multi_t list
    (* Run a tactic and then for each subgoal apply the appropriate tactic
     * from the list to it. This demands that the number of tactics and subgoals
     * be equal
     *)
    | SPLIT of t * t list
    (* This allows a user to try tactics from a list until one succeeds *)
    | CHOOSE of t list
    (* Repeatedly apply a tactic until it fails and then behave as identity *)
    | REPEAT of t
    | ID | FAIL

    (* All type formers have elimination, introduction and equality rules
     * defined for them and the programs they classify. Since there's
     * only one such rule for each type we can provide the user "megatactics"
     * that just pick the only applicable rule for the goal. This reduces the
     * size of the tactic language greatly but it does mean that we need give
     * each mega tactic a lot of configuration data.
     *)
    | INTRO of intro_data
    | ELIM of elim_data
    | EQ of eq_data

    (* Then we have several tactics which don't fit into the above *)
    | FUNEXT
    | CUMULATIVE
    | ASSUMPTION of int

    (* Both of these allow a theorem to make use of previously proven
     * theorems
     *)
    | CUT of Guid.t
    | EXPAND of Guid.t
    (* We can also explicitly suggest what the right answer for a proof is *)
    | WITNESS of Term.t
    (* Then there is a battery of specialized tactics for equalities *)
    | SYM
    | STEP (* This is specifically for CEQ, it lets one step the LHS *)
    | REFL (* This again only applies to CEQ *)
    | SUBST of subst_data

  (* In a pipeline of tactics it's nice to be able to split the current set
   * of goals being built up among several different tactics without breaking
   * the pipeline, for this we use multi_t to express both normal and split steps
   * easily.
   *)
  and multi_t = SINGLE of t | MULTI of t list
end

functor PrimitiveTactics(T : TACTIC) :> PRIMITIVE_TACTICS =
struct
  structure Tactic = T

  structure M = TacticMonad
  open T TacticMonad
  infixr >>= <>

  fun id goal = return {evidence = fn [d] => d, goals = [goal]}
  fun fail _ = M.fail

  fun zipAppOpt xs ys =
    SOME (List.map (fn (a, b) => a b) (ListPair.zipEq (xs, ys)))
      handle _ => NONE

  fun choose (t1, t2) goal = M.choose [fn () => t1 goal, fn () => t2 goal]

  (* joinEvidence is the critical glue for split. The idea is that it
   * takes a bunch of subresults + a derivation building function which
   * uses the evidence those subresults can do. It then takes a list of
   * derivations, splits it up between the subresults, and plumbs the result
   * all the way up to the top level builder.
   *)
  fun joinEvidence top (subresults : result list) ds =
    let
      fun chunk [] _ = []
        | chunk (i :: is) ds =
          List.take (ds, i) :: chunk is (List.drop (ds, i))

      val derivChunks = chunk (List.map (List.length o #goals) subresults) ds
      val SOME evidence = zipAppOpt (List.map #evidence subresults) derivChunks
    in
      top evidence
    end

  fun split (t1, ts : T.t list) goal =
    t1 goal >>= (fn {goals, evidence} =>
    case zipAppOpt ts goals of
        (* If we can't match every goal with a tactic, fail *)
        NONE => M.fail
        (* Otherwise propogate all the subgoals and evidence up *)
      | SOME subresults =>
        sequence subresults >>= (fn subresults =>
        return { goals = List.concat (List.map #goals subresults)
               , evidence = joinEvidence evidence subresults
               }))

  fun next (t1, t2) goal =
    t1 goal >>= (fn {goals, evidence} =>
    sequence (List.map t2 goals) >>= (fn subresults =>
    return { goals = List.concat (List.map #goals subresults)
           , evidence = joinEvidence evidence subresults
           }))
end

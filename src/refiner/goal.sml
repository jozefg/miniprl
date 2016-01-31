(* See goal.sig for docs *)
structure Goal :> GOAL =
struct
  type context = Term.t list
  datatype t = >> of context * Term.t

  fun nth i cxt = Term.lift 0 (i + 1) (List.nth (cxt, i))
end

(* This is the particular implementation of tactics
 * we're going to use throughout the refiner. It uses
 * the goals we've just defined as tactic goals and
 * derivations in the type theory as the sort of derivation
 * that tactics produce.
 *)
structure PrlTactic :> TACTIC
  where type goal = Goal.t
    and type derivation = Derivation.t =
struct
  type derivation = Derivation.t
  type goal = Goal.t

  type 'a choice = (derivation, 'a) TacticMonad.t
  type result = { evidence : derivation list -> derivation
                , goals : goal list
                }
  type t = goal -> result choice
end

(* Given a tactic system, we can define several "structural" sorts
 * of tactic which are quite handy and defined here, outside of the
 * refiner. This also gives a nice flavoring for how exactly the
 * tactic system works.
 *)
signature PRIMITIVE_TACTICS =
sig
  structure Tactic : TACTIC

  (* Simple tactics we can now define: *)
  val id   : Tactic.t
  val fail : Tactic.t

  (* There are several ways of composing tactics together to
   * build larger ones as well
   *)

  (* run one tactic and if it fails, a different one *)
  val choose : Tactic.t * Tactic.t -> Tactic.t
  (* run one tactic and then run another one on all the subgoals *)
  val next   : Tactic.t * Tactic.t -> Tactic.t
  (* run on tactic and feed each subgoal to the matching tactic in the list.
   * There must be exactly the same amount of subgoals and subtactics
   *)
  val split  : Tactic.t * Tactic.t list -> Tactic.t
  (* Run a tactic over and over again until it fails, at which point behave
   * as identity
   *)
  val repeat : Tactic.t -> Tactic.t
end

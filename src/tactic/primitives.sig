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
  val choose : Tactic.t * Tactic.t -> Tactic.t
  val next   : Tactic.t * Tactic.t -> Tactic.t
  val split  : Tactic.t * Tactic.t list -> Tactic.t

end

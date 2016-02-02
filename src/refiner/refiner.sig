(* This is the top level interface to the refiner. You should build up
 * a PrlTactic.t using the Rules structure and then use this function.
 *
 * In particular the average user of the refiner should have no cause
 * to touch another structure in this entire directory, especially
 * Derivation or Extract, all of that should be bundled up here.
 *)
signature REFINER =
sig
  datatype result
    = PROVED of { deriv : Derivation.t
                , extract : Term.t
                }
    | INCOMPLETE of Goal.t list
    | FAILED

  (* prove attempts to prove >> t using the supplied tactics.
   * If the result is proved the derivation is produced and a
   * realizer is extracted and that is returned. If the tactic
   * doesn't finish but still manages to make some progress we
   * return the remaining goals. Finally, if instead the tactic
   * cannot be successfully applied we just return FAILED
   *)
  val prove : Term.t -> PrlTactic.t -> result
end

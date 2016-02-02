(* The refiner is implemented so that it focuses on one theorem at a time.
 * Someone else has to tell it about the configuration of the rest of the
 * theorem prover. For now, this just boils down to needing to be able to
 * tell the refiner when an operator represents a theorem and if so, what it's
 * type and extract are.
 *
 * Some tactics will take a configuration (something of type t) and use it
 * to implement the rule. For example, the rule Cut is designed to introduce
 * previously proven theorems as hypotheses for the current goal. It takes
 * the configuration so that it can validate the type of the previous theorem
 * the user is asking to reuse.
 *)
signature REFINER_CONFIG =
sig
  (* The actual type of a configuration *)
  type t

  (* Create a new empty configuration that has no theorems *)
  val empty : unit -> t

  (* Add a new theorem to a configuration
   *   - tp should be the actual proposition proved
   *   - extract should be the realizer for the proof of the theorem
   *)
  val insert : {name : Guid.t, tp : Term.t, extract : Term.t} -> t -> t

  (* For using a configuration we can query the extract or type of a theorem.
   * If the theorem is not present we return NONE.
   *)
  val typeOf : Guid.t -> t -> Term.t option
  val extractOf : Guid.t -> t -> Term.t option
end

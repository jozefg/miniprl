signature INTERPRETER =
sig
  datatype result = STEP of Term.t | HNF | STUCK


  (* Step the program once according to the structured operational semantics. *)
  val step : Term.t -> result

  (* Run the program to hnf according to the structured operational semantics.
   * If this can be run to a hnf then SOME v is returned. If the term gets stuck
   * somewhere this instead returns NONE
   *)
  val run  : Term.t -> Term.t option
end

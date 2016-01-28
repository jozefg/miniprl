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

  (* Behaves like run but attempts to compute a proper normal form,
   * not just a head normal version. This is useful for debugging
   * especially
   *)
  val normalize : Term.t -> Term.t option
end

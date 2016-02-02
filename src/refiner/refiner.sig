signature REFINER =
sig
  datatype result
    = PROVED of { deriv : Derivation.t
                , extract : Term.t
                }
    | INCOMPLETE of Goal.t list
    | FAILED

  val prove : Term.t -> PrlTactic.t -> result
end

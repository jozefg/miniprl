structure RefinerConfig :> REFINER_CONFIG =
struct
  structure Dict = SplayDict(structure Key = Guid)

  (* Under the hood, we use a splay dictionary mapping guids to all the
   * data as a record
   *)
  type t = {tp : Term.t, extract : Term.t} Dict.dict

  fun empty () = Dict.empty
  fun insert {name, tp, extract} d = Dict.insert d name {tp = tp, extract = extract}
  fun typeOf name (d : t) = Option.map #tp (Dict.find d name)
  fun extractOf name (d : t) = Option.map #extract (Dict.find d name)
end

signature GUID =
sig
  eqtype t

  val compare : t * t -> order
  val toString : t -> string

  val new : unit -> t
  val named : string -> t
end

(* This takes the nice user-level tactic language with the
 * uber tactics and compiles it down to the semantic tactics
 * that tactics/ and refiner/ both build up
 *)
signature COMPILE_SCRIPT =
sig
  val compile : Script.t -> PrlTactic.t
end

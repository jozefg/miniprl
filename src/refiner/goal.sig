(* This explains how we view goals in miniprl. This is what tactics will try to
 * prove and what we'll construct derivations for. The actual type is very simple,
 * you have a list of terms on one side for a context and one term on the other.
 *
 * The reason for using a list is because it's very simple and because we're using
 * De Bruijn indices for variables, this means that we don't need to have
 * explicit names for anything in the context anyways.
 *
 * IMPORTANT: We maintain the invariant throughout all the rules that the things
 * the context are well-formed types with respect to the rest of the context
 * and they may refer to previous occurences in the context.
 *)
signature GOAL =
sig
  (* This describes the "visibility" of a hypothesis. A hidden hypothesis
   * is one for which we may not depend on computationally. This ensures
   * that it can't show up in a realizer for example. It's a useful way to
   * attach data that we need to prove some theorems but would only serve to
   * clutter up the programs which we extrac them to.
   *)
  datatype visible = HIDDEN | VISIBLE

  type context = (visible * Term.t) list
  datatype t = >> of context * Term.t


  (* Grab the nth item (counting starting at 0 and with the front of
   * the list being the first item). This correctly lifts the term
   * as it's being pulled out so that it's well-formed under the
   * context it belongs to. This means that nth (0, H) will *lift the
   * first item in H* so that all the variables in the entry will
   * still point to the correct things in H. The bool argument indicates
   * whether or not we'll accept irrelevant arguments. If it is false
   * and the item indicated in the hypothesis is irrelevant we return
   * NONE, in all other cases SOME ... would be returned.
   *
   * If this is out of bounds we also return NONE
   *)
  val nth : bool -> int -> context -> Term.t option
end

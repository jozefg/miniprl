(* These are all the tactics for manipulating goals to do with
 * the pi type in miniprl. This includes the various rules for
 * actually proving goals of the form ... > Pi x : A. B and
 * goals about proving equality involving Pi, lam, ap, and so
 * on.
 *
 * This means that the module ascribing to PI_RULES is really
 * all that needs to be validated in order to believe that the
 * proof assistant behaves correctly for Pi.
 *)
signature PI_RULES =
sig
  (* For ease of reading, I've duplicated the rules for each of
   * the tactics between here and the tactic. In addition, I've
   * noted the derivation constructor that each tactic uses if
   * there's a clear one.
   *)

  (* H >> Pi x : A. B = Pi x : A'. B' in U(i)
   *   H >> A = A' in U(i)
   *   H, x : A >> B(x) = B'(x) in U(i)
   * Uses: PI_EQ
   *)
  val Eq     : PrlTactic.t

  (* H >> Pi x : A. B
   *   H >> A = A in U(i)
   *   H, x : A >> B
   * Uses: PI_INTRO
   *)
  val Intro  : Utils.universe -> PrlTactic.t

  (* H >> C
   *   H(i) = Pi x : A. B
   *   H >> a = a in A
   *   H, [a/x]B >> C
   * Uses: PI_ELIM
   *)
  val Elim   : Utils.target -> Term.t -> PrlTactic.t

  (* H >> \x. M = \x. N in Pi x : A. B
   *   H >> A = A in U(i)
   *   H, x : A >> M = N in B
   * Uses: LAM_EQ
   *)
  val LamEq  : Utils.universe -> PrlTactic.t

  (* H >> M N = M' N' in B
   *   H >> M = M' in Pi x : A. B'
   *   H >> N = N' in A
   *   H >> [N/x]B' = B in U(i)
   * Uses: AP_EQ
   *)
  val ApEq   : Utils.universe -> Term.t -> PrlTactic.t

  (* H >> M = N in Pi x : A. B
   *   H >> M = M in Pi x : A. B
   *   H >> N = N in Pi x : A. B
   *   H, x : A >> M x = N x in B
   * Uses: FUN_EXT
   *)
  val FunExt : PrlTactic.t
end

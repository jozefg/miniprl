signature NAT_RULES =
sig
  (* H >> nat = nat in U(i)
   *)
  val Eq : PrlTactic.t

  (* H >> nat
   *)
  val Intro : PrlTactic.t

  (* H >> A
   *   H(i) = nat
   *   H, zero = i in nat >> [zero/i]A
   *   H, n : nat, rec : [n/i]A, suc(n) = i : A >> [suc(n)/i]A
   *)
  val Elim : Utils.target -> PrlTactic.t

  (* H >> zero = zero in nat
   *)
  val ZeroEq : PrlTactic.t

  (* H >> suc(n) = suc(m) in nat
   *   H >> n = m in natural
   *)
  val SuccEq : PrlTactic.t

  (* H >> rec(n; z; x.y.s) = rec(n'; z'; x.y.s') in A
   *   H >> n = n' in nat
   *   H >> z = z' in A
   *   H, x : nat, y : A >> s = s' in A
   *)
  val RecEq : PrlTactic.t
end

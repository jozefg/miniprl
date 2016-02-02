structure Utils :> UTILS =
struct
  type universe = int
  type target = int

  exception MalformedEvidence

  open Term
  fun substOpen new i old = Term.lift i 1 (Term.subst new i old)

  fun irrelevant t =
    case t of
        EQ (a, b, t) => true
      | CEQ (a, b) => true
      | VAR i => false
      | LAM b => false
      | AP (f, a) => false
      | PI (a, b) => false
      | PAIR (l, r) => false
      | FST e => false
      | SND e => false
      | SIG (a, b) => false
      | ZERO => false
      | SUCC t => false
      | REC (n, z, s) => false
      | NAT => false
      | TT => false
      | UNIT => true
      | BASE => false
      | UNI i => false
      | PER per => false
      | FIX e => false
      | CUST (id, es) => false

  fun ::: (t, cxt) = (Goal.VISIBLE, t) :: cxt
end

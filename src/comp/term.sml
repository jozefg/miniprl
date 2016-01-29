(* The boring implementation of TERM, see the sig for documentation *)
structure Term :> TERM =
struct
  datatype t
    = VAR of int
    | LAM of t
    | APP of t * t
    | PI  of t * t

    | PAIR of t * t
    | FST of t
    | SND of t
    | SIG of t * t

    | ZERO
    | SUCC of t
    | REC of t * t * t
    | NAT

    | TT
    | UNIT

    | EQ of t * t * t
    | CEQ of t * t
    | BASE
    | UNI of int
    | PER of t
    | FIX of t
    | CUST of Guid.t * t list

  fun lift target i tt =
    case tt of
        VAR j => if j < target then VAR j else VAR (j + i)
      | LAM b => LAM (lift (target + 1) i b)
      | APP (f, a) => APP (lift target i f, lift target i a)
      | PI (a, b) => PI (lift target i a, lift (target + 1) i b)

      | PAIR (l, r) => PAIR (lift target i l, lift target i r)
      | FST e => FST (lift target i e)
      | SND e => SND (lift target i e)
      | SIG (a, b) => SIG (lift target i a, lift (target + 1) i b)

      | ZERO => ZERO
      | SUCC t => SUCC (lift target i t)
      | REC (n, z, s) => REC ( lift target i n
                             , lift target i z
                             , lift (target + 2) i s )
      | NAT => NAT

      | TT => TT
      | UNIT => UNIT

      | EQ (a, b, t) => EQ (lift target i a, lift target i b, lift target i t)
      | CEQ (a, b) => CEQ (lift target i a, lift target i b)
      | BASE => BASE
      | UNI i => UNI i
      | PER per => PER (lift (target + 2) i per)
      | FIX e => FIX (lift (target + 1) i e)
      | CUST (id, es) => CUST (id, List.map (lift target i) es)

  fun lower target i tt =
    case tt of
        VAR j => if j < target then VAR j else VAR (j - i)
      | LAM b => LAM (lower (target + 1) i b)
      | APP (f, a) => APP (lower target i f, lower target i a)
      | PI (a, b) => PI (lower target i a, lower (target + 1) i b)

      | PAIR (l, r) => PAIR (lower target i l, lower target i r)
      | FST e => FST (lower target i e)
      | SND e => SND (lower target i e)
      | SIG (a, b) => SIG (lower target i a, lower (target + 1) i b)

      | ZERO => ZERO
      | SUCC t => SUCC (lower target i t)
      | REC (n, z, s) => REC ( lower target i n
                             , lower target i z
                             , lower (target + 2) i s )
      | NAT => NAT

      | TT => TT
      | UNIT => UNIT

      | EQ (a, b, t) => EQ (lower target i a, lower target i b, lower target i t)
      | CEQ (a, b) => CEQ (lower target i a, lower target i b)
      | BASE => BASE
      | UNI i => UNI i
      | PER per => PER (lower (target + 2) i per)
      | FIX e => FIX (lower (target + 1) i e)
      | CUST (id, es) => CUST (id, List.map (lower target i) es)

  fun subst new i old =
    case old of
        VAR j => if j = i
                 then new
                 else if j > i then VAR (j - 1) else VAR j
      | LAM b => LAM (subst (lift 0 1 new) (i + 1) b)
      | APP (f, a) => APP (subst new i f, subst new i a)
      | PI (a, b) => PI (subst new i a, subst (lift 0 1 new) (i + 1) b)

      | PAIR (l, r) => PAIR (subst new i l, subst new i r)
      | FST e => FST (subst new i e)
      | SND e => SND (subst new i e)
      | SIG (a, b) => SIG (subst new i a, subst (lift 0 1 new) (i + 1) b)

      | ZERO => ZERO
      | SUCC t => SUCC (subst new i t)
      | REC (n, z, s) => REC ( subst new i n
                             , subst new i z
                             , subst (lift 0 2 new) (i + 2) s )
      | NAT => NAT

      | TT => TT
      | UNIT => UNIT

      | EQ (a, b, t) => EQ (subst new i a, subst new i b, subst new i t)
      | CEQ (a, b) => CEQ (subst new i a, subst new i b)
      | BASE => BASE
      | UNI i => UNI i
      | PER per => PER (subst (lift 0 2 new) (i + 2) per)
      | FIX e => FIX (subst (lift 0 1 new) (i + 1) e)
      | CUST (id, es) => CUST (id, List.map (subst new i) es)
end

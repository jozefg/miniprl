structure Interpreter :> INTERPRETER =
struct
  datatype result = STEP of Term.t | HNF | STUCK

  open Term
  (* Step the program once according to the structured operational semantics. *)
  fun step e =
    case e of
        VAR i => HNF
      | LAM b => HNF
      | AP (f, a) => (
        case step f of
            STEP f' => STEP (AP (f', a))
          | STUCK => STUCK
          | HNF =>
            case f of
                LAM b => STEP (subst a 0 b)
              | _ => STUCK
      )
      | PI (a, b) => HNF
      | PAIR (l, r) => HNF
      | FST e => (
        case step e of
            STEP e' => STEP (FST e')
          | STUCK => STUCK
          | HNF =>
            case e of
                PAIR (l, r) => STEP l
              | _ => STUCK
      )
      | SND e => (
        case step e of
            STEP e' => STEP (SND e')
          | STUCK => STUCK
          | HNF =>
            case e of
                PAIR (l, r) => STEP r
              | _ => STUCK
      )
      | SIG (a, b) => HNF
      | ZERO => HNF
      | SUCC e => HNF
      | REC (n, z, s) => (
        case step n of
            STEP n' => STEP (REC (n', z, s))
          | STUCK => STUCK
          | HNF =>
            case n of
                ZERO => STEP z
              | SUCC n' =>
                STEP (subst (REC (n', z, s)) 0 (subst (lift 0 1 n') 0 s))
              | _ => STUCK
      )
      | NAT => HNF
      | TT => HNF
      | UNIT => HNF
      | EQ (a, b, t) => HNF
      | CEQ (a, b) => HNF
      | BASE => HNF
      | UNI i => HNF
      | PER per => HNF
      | FIX e => STEP (Term.subst (FIX e) 0 e)
      | CUST (id, opers) => HNF

  (* Run the program to hnf according to the structured operational semantics. *)
  fun run e =
    case step e of
        STEP e' => run e'
      | HNF => SOME e
      | STUCK => NONE

  (* In order to simplify this code a bit, we define a version of
   * run which raises exceptions rather than NONE and use that to
   * implement normalize. The idea is that this way we don't have
   * to manually propogate all the NONE's around, monad-style, we just
   * let the exn bubble up through the term
   *)
  exception Stuck
  fun runUnsafe e =
    case run e of
        SOME e => e
      | NONE => raise Stuck

  fun normUnsafe e =
    case runUnsafe e of
        VAR i => VAR i
      | LAM b => LAM (normUnsafe b)
      | AP (f, a) => AP (normUnsafe f, normUnsafe a)
      | PI (a, b) => PI (normUnsafe a, normUnsafe b)
      | PAIR (l, r) => PAIR (normUnsafe l, normUnsafe r)
      | FST e => FST (normUnsafe e)
      | SND e => SND (normUnsafe e)
      | SIG (a, b) => SIG (normUnsafe a, normUnsafe b)
      | ZERO => ZERO
      | SUCC e => SUCC (normUnsafe e)
      | REC (n, z, s) => REC (normUnsafe n, normUnsafe z, normUnsafe s)
      | NAT => NAT
      | TT => TT
      | UNIT => UNIT
      | EQ (a, b, t) => EQ (normUnsafe a, normUnsafe b, normUnsafe t)
      | CEQ (a, b) => CEQ (normUnsafe a, normUnsafe b)
      | BASE => BASE
      | UNI i => UNI i
      | PER per => PER (normUnsafe per)
      | FIX e => normUnsafe (Term.subst (FIX e) 0 e)
      | CUST (id, opers) => CUST (id, map normUnsafe opers)

  fun normalize e = SOME (normUnsafe e) handle Stuck => NONE
end

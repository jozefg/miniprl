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

  (* In order to simplify this code a bit, we define a version of
   * run which raises exceptions rather than NONE and use that to
   * implement parallelStep. The idea is that this way we don't have
   * to manually propogate all the NONE's around, monad-style, we just
   * let the exn bubble up through the term
   *)
  exception Stuck
  fun stepUnsafe e =
    case step e of
        STEP e' => e'
      | HNF => e
      | STUCK => raise Stuck

  fun deepStep e =
    case stepUnsafe e of
        VAR i => VAR i
      | LAM b => LAM (deepStep b)
      | AP (f, a) => AP (deepStep f, deepStep a)
      | PI (a, b) => PI (deepStep a, deepStep b)
      | PAIR (l, r) => PAIR (deepStep l, deepStep r)
      | FST e => FST (deepStep e)
      | SND e => SND (deepStep e)
      | SIG (a, b) => SIG (deepStep a, deepStep b)
      | ZERO => ZERO
      | SUCC e => SUCC (deepStep e)
      | REC (n, z, s) => REC (deepStep n, deepStep z, deepStep s)
      | NAT => NAT
      | TT => TT
      | UNIT => UNIT
      | EQ (a, b, t) => EQ (deepStep a, deepStep b, deepStep t)
      | CEQ (a, b) => CEQ (deepStep a, deepStep b)
      | BASE => BASE
      | UNI i => UNI i
      | PER per => PER (deepStep per)
      | FIX e => deepStep (Term.subst (FIX e) 0 e)
      | CUST (id, opers) => CUST (id, map deepStep opers)

  (* TODO: Parallel step involves an equality check. This is so that
   * we can explicitly fail if we don't actually make progress which
   * turns out to be essential for making the stepping tactics usable
   * with repeat. However, this is kind of slow.. Can we do better?
   *)
  fun parallelStep e =
    case SOME (deepStep e) handle Stuck => NONE of
        SOME e' => if e = e' then NONE else SOME e'
      | NONE => NONE

  (* Run the program to hnf according to the structured operational semantics. *)
  fun run e =
    case step e of
        STEP e' => run e'
      | HNF => SOME e
      | STUCK => NONE

  fun normalize e =
   case parallelStep e of
       SOME e' => if e = e' then SOME e else normalize e'
     | NONE => NONE
end

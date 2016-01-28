structure Interpreter :> INTERPRETER =
struct
  datatype result = STEP of Term.t | HNF | STUCK

  open Term
  (* Step the program once according to the structured operational semantics. *)
  fun step e =
    case e of
        VAR i => HNF
      | LAM b => HNF
      | APP (f, a) => (
        case step f of
            STEP f' => STEP (APP (f', a))
          | STUCK => STUCK
          | HNF =>
            case f of
                LAM b => STEP (subst a 0 f)
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
              | SUCC n => STEP (subst (REC (n, z, s)) 0 (subst (lift 0 1 n) 0 s))
              | _ => STUCK
      )
      | NAT => HNF

      | TT => HNF
      | UNIT => HNF

      | EQ (a, b, t) => HNF
      | CEQ (a, b) => HNF
      | BASE => HNF
      | PER per => HNF
      | FIX e => STEP (Term.subst (FIX e) 0 e)
      | CUST (id, opers) => HNF

  (* Run the program to hnf according to the structured operational semantics. *)
  fun run e =
    case step e of
        STEP e' => run e'
      | HNF => SOME e
      | STUCK => NONE
end

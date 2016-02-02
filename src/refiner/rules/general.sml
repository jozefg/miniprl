structure GeneralRules :> GENERAL_RULES =
struct
  open Goal PrlTactic TacticMonad Derivation Term Utils
  infix 3 >>
  infixr >>= <>
  infixr 4 :::

  fun Witness w (cxt >> t) =
    return { goals = [ cxt >> EQ (w, w, t) ]
           , evidence = fn [d] => WITNESS (w, d)
                         | _ => raise MalformedEvidence
           }

  fun Cut config thm (cxt >> t) =
    case RefinerConfig.typeOf thm config of
        SOME t' =>
        return { goals = [ t' ::: cxt >> lift 0 1 t ]
               , evidence = fn [d] => CUT (thm, d)
                             | _ => raise MalformedEvidence
               }
        | NONE => fail

  fun Hyp target (cxt >> t) =
    case nth (irrelevant t) target cxt of
        SOME t' =>
        if t = t'
        (* If the hypothesis was exactly our goal, we're done. *)
        then
          return { goals = []
                 , evidence = fn [] => Derivation.VAR target
                               | _ => raise MalformedEvidence
                 }
        else fail
      | NONE => fail

  (* To make this function a little easier on the eyes, we use
   * an exception to handle the case where the user has applied
   * their custom operator the wrong number of subterms. If this
   * is raised the Unfold rule will catch this, hide it from the user,
   * and just backtrack
   *)
  exception MalformedApplication

  fun expand thm extract t =
    let
      fun go extract t =
        case t of
            VAR j => VAR j
          | LAM b => LAM (go (lift 0 1 extract) b)
          | AP (f, a) => AP (go extract f, go extract a)
          | PI (a, b) => PI (go extract a, go (lift 0 1 extract) b)
          | PAIR (l, r) => PAIR (go extract l, go extract r)
          | FST e => FST (go extract e)
          | SND e => SND (go extract e)
          | SIG (a, b) => SIG (go extract a, go (lift 0 1 extract) b)
          | ZERO => ZERO
          | SUCC t => SUCC (go extract t)
          | REC (n, z, s) => REC ( go extract n
                                 , go extract z
                                 , go (lift 0 2 extract) s )
          | NAT => NAT
          | TT => TT
          | UNIT => UNIT
          | EQ (a, b, t) => EQ (go extract a, go extract b, go extract t)
          | CEQ (a, b) => CEQ (go extract a, go extract b)
          | BASE => BASE
          | UNI i => UNI i
          | PER per => PER (go (lift 0 2 extract) per)
          | FIX e => FIX (go (lift 0 1 extract) e)
          | CUST (id, []) => if id = thm
                             then extract
                             else CUST (id, [])
          | CUST (id, xs) => if id = thm
                             then raise MalformedApplication
                             else CUST (id, xs)
    in
      go extract t
    end

  fun Unfold config thm (cxt >> t) =
    case RefinerConfig.extractOf thm config of
        NONE => fail
      | SOME e =>
        let
          val exp = expand thm e
        in
          return { goals = [ List.map (fn (v, t) => (v, exp t)) cxt >> exp t ]
                 (* Expansion is silent in the derivation so you
                  * don't see a change in the derivation
                  *)
                 , evidence = fn [d] => d
                               | _ => raise MalformedEvidence
                 }
        end handle MalformedApplication => fail
end

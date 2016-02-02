structure GeneralRules :> GENERAL_RULES =
struct
  open Goal PrlTactic TacticMonad Derivation Term Utils
  infix 3 >>
  infixr >>= <>
  infixr 4 :::

  fun Witness w (cxt >> t) =
    return { goals = [ cxt >> EQ (w, w, t) ]
           , evidence = fn [d] => WITNESS d
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
                 , evidence = fn [] => VAR target
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
  fun Unfold config thm (cxt >> t) = raise Fail ""
end

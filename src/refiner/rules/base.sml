structure BaseRules :> BASE_RULES =
struct
  open Goal PrlTactic TacticMonad Derivation Term Utils
  infix 3 >>
  infixr >>= <>

  fun Eq (cxt >> t) =
    case t of
        EQ (BASE, BASE, UNI i) =>
        return { goals = []
               , evidence = fn [] => BASE_EQ
                             | _ => raise MalformedEvidence
               }
      | _ => fail

  fun MemEq (cxt >> t) =
    case t of
        EQ (a, b, BASE) =>
        let
          (* We need to check that all free variables in a and b
           * are variables of type base to ensure the functionality
           * of our sequent judgment. Therefore we just gather them
           * all up here and assert it before applying our rule
           *)
          val vars = freevars a @ freevars b
        in
          if List.all (fn v => nth v cxt = BASE) vars
          then
            return { goals = [ cxt >> CEQ (a, b) ]
                   , evidence = fn [d] => BASE_MEM_EQ d
                                 | _ => raise MalformedEvidence
                   }
          else fail
        end
      | _ => fail

  fun ElimEq target (cxt >> t) =
    case nth target cxt of
        EQ (a, b, BASE) =>
        (* Note that since we're adding to the front of the context we have to
         * lift t up to maintain its variables
         *)
        return { goals = [CEQ (a, b) :: cxt >> lift 0 1 t ]
               , evidence = fn [d] => BASE_ELIM_EQ (target, d)
                             | _ => raise MalformedEvidence
               }
     | _ => fail
end

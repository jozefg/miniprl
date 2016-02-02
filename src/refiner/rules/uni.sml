structure UniRules :> UNI_RULES =
struct
  open Goal PrlTactic TacticMonad Derivation Term Utils
  infix 3 >>
  infixr >>= <>

  fun Eq (cxt >> t) =
    case t of
        EQ (UNI i, UNI j, UNI k) =>
        (* Check that really this is U(i) = U(i) in U(i + 1) so we can
         * apply the rule
         *)
        if i = j andalso i + 1 = k
        then
          return
              { goals = []
              , evidence = fn [] => UNI_EQ
                            | _ => raise MalformedEvidence
              }
        else fail
      | _ => fail

  fun Cumulative (cxt >> t) =
    case t of
        EQ (A, B, UNI i) =>
        (* We cannot have universe levels lower than 0 so
         * we need a quick check to maintain this.
         *)
        if i > 0
        then
          return
              { goals = [ cxt >> EQ (A, B, UNI (i - 1)) ]
              , evidence = fn [d] => CUMULATIVE d
                            | _ => raise MalformedEvidence
              }
        else fail
      | _ => fail

end

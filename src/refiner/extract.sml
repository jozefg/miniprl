structure Extract :> EXTRACT =
struct
  structure D = Derivation
  open Term

  fun extract deriv =
      case deriv of
          D.PI_EQ _ => TT
        | D.LAM_EQ _ => TT
        | D.AP_EQ _ => TT
        | D.FUN_EXT _ => TT
        | D.PAIR_EQ _ => TT
        | D.FST_EQ _ => TT
        | D.SND_EQ _ => TT
        | D.NAT_EQ => TT
        | D.SIG_EQ _ => TT
        | D.ZERO_EQ => TT
        | D.SUCC_EQ _ => TT
        | D.REC_EQ _ => TT
        | D.UNIT_EQ => TT
        | D.TT_EQ => TT
        | D.EQ_EQ _ => TT
        | D.EQ_MEM_EQ _ => TT
        | D.EQ_SYM _ => TT
        | D.CEQ_EQ _ => TT
        | D.CEQ_MEM_EQ _ => TT
        | D.CEQ_SYM _ => TT
        | D.CEQ_STEP _ => TT
        | D.CEQ_REFL => TT
        | D.BASE_EQ => TT
        | D.BASE_MEM_EQ _ => TT
        | D.PER_EQ _ => TT
        | D.PER_MEM_EQ _ => TT
        | D.UNI_EQ => TT
        | D.CEQ_SUBST (_, _, d) => extract d
        | D.EQ_SUBST (_, _, _, _, d) => extract d
        | D.PI_INTRO (_, _, d) => LAM (extract d)
        | D.PI_ELIM (fIdx, arg, _, d) => subst (AP (VAR fIdx, arg)) 0 (extract d)
        | D.SIG_INTRO (_, t, _, d, _) => PAIR (t, extract d)
        | D.SIG_ELIM (sigIdx, d) =>
          subst (SND (VAR sigIdx)) 0
                (subst (FST (VAR (sigIdx + 1))) 0 (extract d))
        | D.NAT_INTRO => ZERO
        | D.NAT_ELIM (i, d1, d2) =>
          REC ( VAR i
              , subst TT 0 (extract d1)
              , subst TT 0 (lift 1 1 (extract d2)))
        | D.UNIT_INTRO => TT
        | D.BASE_ELIM_EQ (i, d) => subst TT 0 (extract d)
        | D.CUMULATIVE _ => TT
        | D.WITNESS (t, _) => t
        | D.CUT (id, t) => subst (CUST (id, [])) 0 (extract t)
        | D.VAR i => VAR i
end

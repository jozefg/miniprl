(* Here is the actual implementation of derivations. Later on this will
 * contain things like a pretty printer but for now it's just the datatype.
 *)
structure Derivation :> DERIVATION =
struct
  datatype t
    = PI_EQ of t * t (* BINDS *)
    | PI_INTRO of int * t * t (* BINDS *)
    | PI_ELIM of int * Term.t * t * t (* BINDS *)
    | LAM_EQ of int * t * t (* BINDS *)
    | AP_EQ of int * Term.t * t * t * t (* We require the function type to be provided *)
    | FUN_EXT of t * t * t (* BINDS *)
    | SIG_EQ of t * t * t (* BINDS *)
    | SIG_INTRO of Term.t * t * t
    | SIG_ELIM of int * t (* BINDS *)
    | PAIR_EQ of t * t
    | FST_EQ of Term.t * t
    | SND_EQ of Term.t * t
    | NAT_EQ of int
    | NAT_INTRO
    | NAT_ELIM of int * t * t (* BINDS *)
    | ZERO_EQ
    | SUCC_EQ of t
    | REC_EQ of t * t * t (* BINDS *)
    | UNIT_EQ of int
    | TT_EQ
    | UNIT_INTRO
    | EQ_EQ of t * t * t
    | EQ_MEM_EQ of t
    | EQ_SYM of t
    | EQ_SUBST of int * Term.t * t * t * t (* BINDS *)
    | CEQ_EQ of t * t
    | CEQ_MEM_EQ of t
    | CEQ_SYM of t
    | CEQ_SUBST of int * Term.t * t * t
    | CEQ_STEP of t
    | CEQ_REFL
    | BASE_EQ
    | BASE_MEM_EQ of t
    | BASE_ELIM_EQ of int * t (* BINDS *)
    | UNI_EQ
    | CUMULATIVE
    | PER_EQ of t * t * t * t * t * t (* BINDS *) (* haha cries *)
    | PER_MEM_EQ of int * t * t * t * t
    | WITNESS of Term.t * t
    | CUT of Guid.t * t
    | VAR of int
end

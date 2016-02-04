(* For now this is just the implementation of the script
 * datatype, but later it will hopefully include a parser or
 * pretty printer so that these things can be more easily viewed.
 *)
structure Script :> SCRIPT =
struct
  type intro_data = { term : Term.t option
                    , uni  : int option
                    }
  type elim_data = { target : int
                   , term : Term.t option
                   }
  type eq_data = { term : Term.t option
                 , uni : int option
                 }
  type subst_data = { uni : int option
                    , pattern : Term.t
                    , equality : Term.t
                    }
  datatype t
    = SEQUENCE of multi_t list
    | SPLIT of t * t list
    | CHOOSE of t list
    | ID | FAIL
    | REPEAT of t
    | INTRO of intro_data
    | ELIM of elim_data
    | EQ of eq_data
    | FUNEXT
    | CUMULATIVE
    | ASSUMPTION of int
    | CUT of Guid.t
    | EXPAND of Guid.t
    | WITNESS of Term.t
    | SYM
    | STEP
    | REFL
    | SUBST of subst_data
  and multi_t = SINGLE of t | MULTI of t list
end

structure Utils :> UTILS =
struct
  type universe = int
  type target = int

  exception MalformedEvidence

  fun substOpen new i old = Term.lift i 1 (Term.subst new i old)
end

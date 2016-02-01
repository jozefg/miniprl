(* This is the fundamental piece of evidence in the refinement logic.
 * We have one way of forming a derivation for each rule in our PRL
 * and we'll create derivations each time we construct a proof in our logic.
 * More over, each derivation can be "extracted" into a computational
 * realizer.
 *
 * Unforunately, derivations all have a binding structure which means more
 * De Bruijn indices.
 *)
signature DERIVATION =
sig
  (* I've annotated each derivation with the rule I intend it
   * to represent for motivation, this will be mechanized later
   * in the refiner by a tactic in rules/.
   *
   * The peculiar notation of H >> ... should be understood
   * as \Gamma \vdash in many ways but looks significantly better
   * in ASCII :)
   *
   * IMPORTANT: Derivations in here contain terms (for things like application)
   * we silently allow to point to binding sites. Note that most occurences
   * of ints specify universe levels (i in U(i))
   *)
  datatype t
    (* H >> Pi x : A. B = Pi x : A'. B' in U(i)
     *   H >> A = A' in U(i)
     *   H, x : A >> B(x) = B'(x) in U(i)
     *)
    = PI_EQ of t * t (* BINDS *)
    (* H >> Pi x : A. B
     *   H >> A = A in U(i)
     *   H, x : A >> B
     *)
    | PI_INTRO of int * t * t (* BINDS *)
    (* H >> C
     *   H(i) = Pi x : A. B
     *   H >> a = a in A
     *   H, [a/x]B >> C
     *)
    | PI_ELIM of int * Term.t * t * t (* BINDS *)
    (* H >> \x. M = \x. N in Pi x : A. B
     *   H >> A = A in U(i)
     *   H, x : A >> M = N in B
     *)
    | LAM_EQ of int * t * t (* BINDS *)
    (* H >> M N = M' N' in B
     *   H >> M = M' in Pi x : A. B'
     *   H >> N = N' in A
     *   H >> [N/x]B' = B in U(i)
     *)
    | AP_EQ of int * Term.t * t * t * t (* We require the function type to be provided *)
    (* H >> M = N in Pi x : A. B
     *   H >> M = M in Pi x : A. B
     *   H >> N = N in Pi x : A. B
     *   H, x : A >> M x = N x in B
     *)
    | FUN_EXT of t * t * t (* BINDS *)

    (* H >> Sig x : A. B = Sig x : A'. B' in U(i)
     *   H >> A = A' in U(i)
     *   H, x : A >> B = B' in U(i)
     *)
    | SIG_EQ of t * t (* BINDS *)
    (* H >> Sig x : A. B
     *   H >> a = a in A
     *   H >> [a/x]B
     *   H, a : A >> B = B in U(i)
     *)
    | SIG_INTRO of int * Term.t * t * t * t (* BINDS *)
    (* H >> C
     *   H(i) = Sig x : A. B
     *   H, x : A, y : B >> C
     *)
    | SIG_ELIM of int * t (* BINDS *)
    (* H >> pair(a; b) = pair(a'; b') in Sig x : A. B
     *  H >> a = a' in A
     *  H >> b = b' in [a/x]B
     *  H, x : A >> B = B in U(i)
     *)
    | PAIR_EQ of int * t * t * t (* BINDS *)
    (* H >> fst(a) = fst(a') in A
     *   H >> a = a' in Sig x : A. B
     *)
    | FST_EQ of Term.t * t
    (* H >> snd(a) = snd(a') in B
     *   H >> a = a' in Sig x : A. B'
     *   H >> [fst(a)/x]B' = B in U(i)
     * (Note that we need to lift B in DeBruijn)
     *)
    | SND_EQ of int * Term.t * t * t

    (* H >> nat = nat in U(i)
     *)
    | NAT_EQ
    (* H >> nat
     *)
    | NAT_INTRO
    (* H >> A
     *   H(i) = nat
     *   H, zero = i in nat >> [zero/i]A
     *   H, n : nat, rec : [n/i]A, suc(n) = i : A >> [suc(n)/i]A
     *)
    | NAT_ELIM of int * t * t (* BINDS *)
    (* H >> zero = zero in nat
     *)
    | ZERO_EQ
    (* H >> suc(n) = suc(m) in nat
     *   H >> n = m in natural
     *)
    | SUCC_EQ of t
    (* H >> rec(n; z; x.y.s) = rec(n'; z'; x.y.s') in A
     *   H >> n = n' in nat
     *   H >> z = z in A
     *   H, x : nat, y : A >> s = s' in A
     *)
    | REC_EQ of t * t * t (* BINDS *)

    (* H >> unit = unit in U(i)
     *)
    | UNIT_EQ
    (* H >> tt = tt in unit
     *)
    | TT_EQ
    (* H >> unit
     *)
    | UNIT_INTRO

    (* H >> (a = b in A) = (a' = b' in A') in U(i)
     *   H >> A = A' in U(i)
     *   H >> a = a' in A
     *   H >> b = b' in A'
     *)
    | EQ_EQ of t * t * t
    (* H >> tt = tt in (a = b in A)
     *   H >> a = b in A
     *)
    | EQ_MEM_EQ of t
    (* H >> a = b in A
     *   H >> b = a in A
     *)
    | EQ_SYM of t
    (* H >> [a/x]C
     *   H, x : A >> C' in U(i)
     *   H >> a = b in A
     *   H >> [b/x]C
     *)
    | EQ_SUBST of int * Term.t * t * t * t (* BINDS *)

    (* H >> (a ~ b) = (a' ~ b') in U(i)
     *   H >> a ~ a'
     *   H >> b ~ b'
     *)
    | CEQ_EQ of t * t
    (* H >> tt = tt in (a ~ b)
     *   H >> a ~ b
     *)
    | CEQ_MEM_EQ of t
    (* H >> a ~ b
     *   H >> b ~ a
     *)
    | CEQ_SYM of t
    (* H >> [a/x]C
     *   H >> a ~ b
     *   H >> [b/x]C
     *)
    | CEQ_SUBST of int * Term.t * t * t
    (* H >> a ~ b
     *   a |-> a'
     *   H >> a' ~ b
     *)
    | CEQ_STEP of t
    (* H >> a ~ a
     *)
    | CEQ_REFL

    (* H >> base = base in U(i)
     *)
    | BASE_EQ
    (* H >> a = b in base
     *   x in FV(a) => H(x) = base
     *   x in FV(b) => H(x) = base
     *   H >> a ~ b
     *)
    | BASE_MEM_EQ of t
    (* H >> C
     *   H(i) = (a = b in base)
     * H, a ~ b >> C
     *)
    | BASE_ELIM_EQ of int * t (* BINDS *)

    (* H >> U(i) = U(i) in U(i + 1)
     *)
    | UNI_EQ
    (* H >> A = B in U(i + 1)
     *   H >> A = B in U(i)
     *)
    | CUMULATIVE
    (* H >> per(x.y.A) = per(x.y.A') in U(i)
     *   H, x : base, y : base >> A = A in U(i)
     *   H, x : base, y : base >> A' = A' in U(i)
     *   H, x : base, y : base, z : A >> A'
     *   H, x : base, y : base, z : A' >> A
     *   H, a : base, b : base, z : [a, b/x, y]A >> [b, a/x, y]A
     *   H, a : base, b : base, c : base, z1 : [a, b/x, y]A, z2 : [b, c/x, y]A
     *      >> [a, c/x, y]A
     *)
    | PER_EQ of t * t * t * t * t * t (* BINDS *) (* haha cries *)
    (* H >> a = b in per(x.y.A)
     *   H >> a in base
     *   H >> b in base
     *   H >> [a, b/x, y]A
     *   H >> per(x.y.A) = per(x.y.A) in U(i)
     *)
    | PER_MEM_EQ of int * t * t * t * t

    (* H >> C
     *   H >> t = t in C *)
    | WITNESS of Term.t * t
    (* H >> C
     *   opid is a lemma proving L
     *   H, L >> C*)
    | CUT of Guid.t * t (* BINDS *)
    (* H >> C
     *   H(i) = C
     *)
    | VAR of int
end

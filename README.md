## miniprl

miniprl is an attempt to construct a proof assignment logic (a PRL)
with the bare minimum set of features to illustrate the concepts. It
includes the basic type formers of Martin-Löf's type theories as well
as a few unique to Computational Type Theory such as per types and
Howe's computational equality.

This project is heavily inspired by [JonPRL][jonprl] and
[Nuprl][nuprl]. These constitute "real" implementations of this flavor
of type theory and are suitable for actually proving theorems.  This
particular implementation is designed for people curious about how to
build such things. Everything is designed to be as modular as possible
without compromising simplicity. Documentation of the internals is an
extremely high priority for this project; if you're confused by
something, say something!

As an overview,

 - The core programming language is build up in `src/comp`

    Start with this. It will tell you the programming language that
    the refiner actually... refines. While it's not super important to
    understand exactly how the interpreter works (it's a fairly naive
    substitution based implementation) it is important to realize that
    we're using De Bruijn indices for all of our binding.

 - The basics of the tactic language is in `src/tactic`

    Read this before you read the refiner. After all, the refiner is
    "just" an implementation of the `TACTIC` signature and the
    definition of a bunch of tactics in the end. The inspiration for
    this implementation is very clearly [sml-lcf][sml-lcf] but I've
    opted to avoid using `exn`s for control flow and instead to use a
    continuation-y monad. It's uglier but I think the extra
    explicitness helps.

 - The refiner is built in `src/refiner`. This defines the real logic
   of the language.

    The refiner is divided into a few different components. There's a
    module for derivations exhaustively list all the possible
    operations we can use to manipulate goals during the course of a
    proof. To actually perform these manipulations we also have a lot
    of definitions of different primitive tactics (rules). The idea
    being that everything else in the proof assistant will build on
    top of these primitive rules and thus the entire trusted base of
    the proof assistant is contained in `src/refiner/rules`. Once we
    construct a derivation using those rules we can extract it to a
    runnable program using `extract.sml`. All of this process is
    bundled up in the `Refiner` module.

 - In `src/interactive-kernel` all of the appropriate features are
   glued together

By tying everything together as a plain old SML library we hope that
miniprl will be easy to extend with different interfaces. This means
that plain old SML code can spin up a session with the refiner and add
new definitions, prove new theorems (interactively!), and query for
derivations and extracts.

miniprl is pure SML '97, it includes CM files for easy use with SML/NJ
but mlb features will be added as soon as there's an interesting
frontend that makes sense to use as a binary.

Danny.

[jonprl]: http://www.jonprl.org
[nuprl]: http://www.nuprl.org
[sml-lcf]: http://www.github.com/jonprl/sml-lcf

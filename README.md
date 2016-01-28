## miniprl

miniprl is an attempt to construct a proof assignment logic (a PRL)
with the bare minimum set of features to illustrate the concepts. It
includes the basic type formers of Martin-Löf's type theories as well
as a few unique to Computational Type Theory such as per types and
Howe's computational equality.

This particular implementation is designed for people curious about
how to build such things. Everything is designed to be as modular as
possible without compromising simplicity. As an overview,

 - The core programming language is build up in `src/comp`
 - The basics of the tactic language is in `src/tactic`
 - The refiner is built in `src/refiner`. This defines the real logic
   of the language
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

testing of typechecker
editing combinators
editor backend


-- a layout tree uses hashes, not names
-- merely a convention of the editor that it
-- transitively updates dependent hashes
-- that is, given:
-- y = 42
-- f x = x + y
-- the editor just adopts the convention that
-- updating `y` will update `f` to point to the
-- new hash of the thing called `y`, rather than
-- continuing to point at old hash
--

-- need path and action type for ADTs
-- implement node server locally

pretty awesome that an immutable codebase means we only have to
typecheck a tiny bit of code at a time


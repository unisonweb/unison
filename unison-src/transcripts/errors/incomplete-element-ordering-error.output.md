Exception when running incomplete-element-ordering-error.md: 🐞

Hashing failed because cyclic definitions because the definitions could not be completely ordered.
This happens when multiple definitions in a mutually recursive cycle are identical except
for references to other elements in the same cycle.
If all elements are identical, consider simple recursion instead of mutual recursion,
If mutual recursion is required, you may disambiguate identical definitions by
adding a dummy comment like:
_ = "this is the foo definition"


This is a Unison bug and you can report it here:

https://github.com/unisonweb/unison/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+E253299+

Bug reference: E253299

If there's already an issue with this reference, you can give a 👍
on the issue to let the team know you encountered it, and you can add
any additional details you know of to the issue.

CallStack (from HasCallStack):
  error, called at src/Unison/Hashing/V2/ABT.hs:42:14 in unison-hashing-v2-0.0.0-8lPXbJPlm6sDj9SGQtF5JE:Unison.Hashing.V2.ABT
  crashOnHashingFailure, called at src/Unison/UnisonFile.hs:281:17 in unison-parser-typechecker-0.0.0-E7FuwoDrRadBVo69eIX64V:Unison.UnisonFile
  typecheckedUnisonFile, called at src/Unison/FileParsers.hs:323:7 in unison-parser-typechecker-0.0.0-E7FuwoDrRadBVo69eIX64V:Unison.FileParsers
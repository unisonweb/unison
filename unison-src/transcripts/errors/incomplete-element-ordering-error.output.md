Exception when running incomplete-element-ordering-error.md: 🐞

🐞

Sorry, you've encountered a weird situation that we are aware of and are currently working on a fix for.
I'll explain what happened and how you can work around it.

The following cyclic definitions could not be completely ordered:
  User "bar", User "foo"
This happens when multiple definitions in a mutually recursive cycle have a very similar structure.

You can work around this by restructuring them to be less similar, e.g. by adding a pure expression to distinguish them, like:
_ = "this is the foo definition"


This is a Unison bug and you can report it here:

https://github.com/unisonweb/unison/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+E253299+

Bug reference: E253299

If there's already an issue with this reference, you can give a 👍
on the issue to let the team know you encountered it, and you can add
any additional details you know of to the issue.

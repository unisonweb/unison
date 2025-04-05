---
name: Error message suggestion
about: Suggest improved wording or design for an error message
title: ''
labels: error-message
assignees: ''

---

**What's the message you're seeing?**
Please paste from your terminal or paste a screenshot, e.g:
```
project/alice> merge /bob

  On project/alice, bar and foo are not aliases, but they used
  to be.

```

**What would a better version look like?**
```
Sorry, I wasn't able to perform the merge:

On the merge ancestor, bar and foo were aliases for the same definition; but on project/alice the names have different definitions currently. I'd need just a single new definition to use in their dependents when I merge.

Please fix up project/alice to resolve this. For example,

    * update the definitions to be the same again, so that there's nothing for me to decide.
    * rename or delete one of the definitions; I'll use the remaining name when propagating updates, 
       and you can change the name back after the merge.

```

Environment (please complete the following information):

* `ucm --version` [e.g. "0.5.21", or "1cb2437 (built on 2024-06-03)"]
* OS/Architecture: [e.g. "macOS 14.5, Intel"]
* Browser, if applicable: [e.g. "chrome 125.0.6422.142"] (Version numbers are typically found the about menu option)

First we define some simple terms

``` unison
x = ()
y = ((), ())
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + x : ()
  + y : ((), ())

  Run `update` to apply these changes to your codebase.
```

And then add them to the default project & branch.

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now, we do different definitions in an explicit project

``` unison
q = ()
r = ((), ())
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + q : ()
      (also named x)
  + r : ((), ())
      (also named y)

  Run `update` to apply these changes to your codebase.
```

And then add them to the default project & branch.

**NB**: The implicit `load` occurs in the previous project & branch (which is why it says “also named” in the block above here). But this is also the case with explicit project context, so if it’s a problem, it’s outside the scope of this feature.

``` ucm
other/trunk> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now, later UCM blocks should maintain the last-mentioned project.

``` ucm
> view q

  q : ()
  q = ()

> view r

  r : ((), ())
  r = ((), ())
```

And it shouldn’t contain these definitions

``` ucm :error
> view x

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    x
```

``` ucm :error
> view y

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    y
```

But the default project & branch should be “scratch/main”, so we can see the original definitions there:

``` ucm
scratch/main> view x

  x : ()
  x = ()

scratch/main> view y

  y : ((), ())
  y = ((), ())

> view y

  y : ((), ())
  y = ((), ())
```

And the other project’s definitions should no longer be available.

``` ucm :error
> view q

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    q
```

``` ucm :error
> view r

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    r
```

Creating a project should also change the current project & branch.

``` ucm
> project.create yet-another

  🎉 I've created the project yet-another.

  I'll now fetch the latest version of the base Unison
  library...

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.

  Write your first Unison code with UCM:

    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `update` to save it to your new project.

  🎉 🥳 Happy coding!
```

The definitions from other projects shouldn’t be here.

``` ucm :error
> view x

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    x
```

``` ucm :error
> view r

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    r
```

So add some definitions to the created project

``` unison
a = ()
b = ((), ())
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + a : ()
  + b : ((), ())

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view a

  a : ()
  a = ()

> view b

  b : ((), ())
  b = ((), ())
```

We shouldn’t see these new definitions in our other projects

``` ucm :error
scratch/main> view a

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    a
```

``` ucm :error
other/trunk> view a

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    a
```

But if we explicitly use the new name, we’re back to our project.

``` ucm
yet-another/main> view a

  a : ()
  a = ()

> view b

  b : ((), ())
  b = ((), ())
```

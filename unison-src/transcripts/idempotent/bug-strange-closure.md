``` ucm :hide
> builtins.mergeio lib.builtins

> load unison-src/transcripts-using-base/doc.md.files/syntax.u
```

We can display the guide before and after adding it to the codebase:

```` ucm
> display doc.guide

  # Unison computable documentation

    # Basic formatting
    
      Paragraphs are separated by one or more blanklines.
      Sections have a title and 0 or more paragraphs or other
      section elements.
    
      Text can be bold, *italicized*, ~~strikethrough~~, or
      `monospaced` (or `monospaced`).
    
      You can link to Unison terms, types, and external URLs:
    
      * An external url
      * Some is a term link; Optional is a type link
      * A named type link and a named term link. Term links are
        handy for linking to other documents!
    
      You can use `{{ .. }}` to escape out to regular Unison
      syntax, for instance __not bold__. This is useful for
      creating documents programmatically or just including
      other documents.
    
      *Next up:* lists

    # Lists
    
      # Bulleted lists
      
        Bulleted lists can use `+`, `-`, or `*` for the bullets
        (though the choice will be normalized away by the
        pretty-printer). They can be nested, to any depth:
      
        * A
        * B
        * C
          * C1
          * C2
    
      # Numbered lists
      
        1. A
        2. B
        3. C
      
        The first number of the list determines the starting
        number in the rendered output. The other numbers are
        ignored:
      
        10. A
        11. B
        12. C
      
        Numbered lists can be nested as well, and combined with
        bulleted lists:
      
        1. Wake up.
           * What am I doing here?
           * In this nested list.
        2. Take shower.
        3. Get dressed.

    # Evaluation
    
      Expressions can be evaluated inline, for instance `2`.
    
      Blocks of code can be evaluated as well, for instance:
    
          id x = x
          id (sqr 10)
          ⧨
          100
    
      also:
    
          match 1 with
            1 -> "hi"
            _ -> "goodbye"
          ⧨
          "hi"
    
      To include a typechecked snippet of code without
      evaluating it, you can do:
    
          use Nat *
          cube : Nat -> Nat
          cube x = x * x * x

    # Including Unison source code
    
      Unison definitions can be included in docs. For instance:
    
          structural type Optional a = None | Some a
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      Some rendering targets also support folded source:
    
          structural type Optional a = None | Some a
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      You can also include just a signature, inline, with
      `sqr : Nat -> Nat`, or you can include one or more
      signatures as a block:
    
          sqr : Nat -> Nat
      
          Nat.+ : Nat -> Nat -> Nat
    
      Or alternately:
    
          List.map : (a ->{e} b) -> [a] ->{e} [b]
    
      # Inline snippets
      
        You can include typechecked code snippets inline, for
        instance:
      
        * `f x Nat.+ sqr 1` - the `2` says to ignore the first
          two arguments when rendering. In richer renderers, the
          `sqr` link will be clickable.
        * If your snippet expression is just a single function
          application, you can put it in double backticks, like
          so: `sqr x`. This is equivalent to `sqr x`.

    # Non-Unison code blocks
    
      Use three or more single quotes to start a block with no
      syntax highlighting:
    
      ``` raw
         _____     _             
        |  |  |___|_|___ ___ ___ 
        |  |  |   | |_ -| . |   |
        |_____|_|_|_|___|___|_|_|
        
      ```
    
      You can use three or more backticks plus a language name
      for blocks with syntax highlighting:
    
      ``` Haskell
      -- A fenced code block which isn't parsed by Unison
      reverse = foldl (flip (:)) []
      ```
    
      ``` Scala
      // A fenced code block which isn't parsed by Unison
      def reverse[A](xs: List[A]) = 
        xs.foldLeft(Nil : List[A])((acc,a) => a +: acc)
      ```

    There are also asides, callouts, tables, tooltips, and more.
    These don't currently have special syntax; just use the
    `{{ }}` syntax to call these functions directly.
    
        docAside : Doc2 -> Doc2
    
        docCallout : Optional Doc2 -> Doc2 -> Doc2
    
        docBlockquote : Doc2 -> Doc2
    
        docTooltip : Doc2 -> Doc2 -> Doc2
    
        docTable : [[Doc2]] -> Doc2
    
    This is an aside. (
    Some extra detail that doesn't belong in main text. )
    
      | This is an important callout, with no icon.
    
      | 🌻
      | 
      | This is an important callout, with an icon. The text
      | wraps onto multiple lines.
    
    > "And what is the use of a book," thought Alice, "without
    > pictures or conversation?"
    > 
    > *Lewis Carroll, Alice's Adventures in Wonderland*
    
    Hover over me
    
    a           b           A longer paragraph that will split
                            onto multiple lines, such that this
                            row occupies multiple lines in the
                            rendered table.
    Some text   More text   Zounds!

> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> display doc.guide

  # Unison computable documentation

    # Basic formatting
    
      Paragraphs are separated by one or more blanklines.
      Sections have a title and 0 or more paragraphs or other
      section elements.
    
      Text can be bold, *italicized*, ~~strikethrough~~, or
      `monospaced` (or `monospaced`).
    
      You can link to Unison terms, types, and external URLs:
    
      * An external url
      * Some is a term link; Optional is a type link
      * A named type link and a named term link. Term links are
        handy for linking to other documents!
    
      You can use `{{ .. }}` to escape out to regular Unison
      syntax, for instance __not bold__. This is useful for
      creating documents programmatically or just including
      other documents.
    
      *Next up:* lists

    # Lists
    
      # Bulleted lists
      
        Bulleted lists can use `+`, `-`, or `*` for the bullets
        (though the choice will be normalized away by the
        pretty-printer). They can be nested, to any depth:
      
        * A
        * B
        * C
          * C1
          * C2
    
      # Numbered lists
      
        1. A
        2. B
        3. C
      
        The first number of the list determines the starting
        number in the rendered output. The other numbers are
        ignored:
      
        10. A
        11. B
        12. C
      
        Numbered lists can be nested as well, and combined with
        bulleted lists:
      
        1. Wake up.
           * What am I doing here?
           * In this nested list.
        2. Take shower.
        3. Get dressed.

    # Evaluation
    
      Expressions can be evaluated inline, for instance `2`.
    
      Blocks of code can be evaluated as well, for instance:
    
          id x = x
          id (sqr 10)
          ⧨
          100
    
      also:
    
          match 1 with
            1 -> "hi"
            _ -> "goodbye"
          ⧨
          "hi"
    
      To include a typechecked snippet of code without
      evaluating it, you can do:
    
          use Nat *
          cube : Nat -> Nat
          cube x = x * x * x

    # Including Unison source code
    
      Unison definitions can be included in docs. For instance:
    
          structural type Optional a = None | Some a
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      Some rendering targets also support folded source:
    
          structural type Optional a = None | Some a
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      You can also include just a signature, inline, with
      `sqr : Nat -> Nat`, or you can include one or more
      signatures as a block:
    
          sqr : Nat -> Nat
      
          Nat.+ : Nat -> Nat -> Nat
    
      Or alternately:
    
          List.map : (a ->{e} b) -> [a] ->{e} [b]
    
      # Inline snippets
      
        You can include typechecked code snippets inline, for
        instance:
      
        * `f x Nat.+ sqr 1` - the `2` says to ignore the first
          two arguments when rendering. In richer renderers, the
          `sqr` link will be clickable.
        * If your snippet expression is just a single function
          application, you can put it in double backticks, like
          so: `sqr x`. This is equivalent to `sqr x`.

    # Non-Unison code blocks
    
      Use three or more single quotes to start a block with no
      syntax highlighting:
    
      ``` raw
         _____     _             
        |  |  |___|_|___ ___ ___ 
        |  |  |   | |_ -| . |   |
        |_____|_|_|_|___|___|_|_|
        
      ```
    
      You can use three or more backticks plus a language name
      for blocks with syntax highlighting:
    
      ``` Haskell
      -- A fenced code block which isn't parsed by Unison
      reverse = foldl (flip (:)) []
      ```
    
      ``` Scala
      // A fenced code block which isn't parsed by Unison
      def reverse[A](xs: List[A]) = 
        xs.foldLeft(Nil : List[A])((acc,a) => a +: acc)
      ```

    There are also asides, callouts, tables, tooltips, and more.
    These don't currently have special syntax; just use the
    `{{ }}` syntax to call these functions directly.
    
        docAside : Doc2 -> Doc2
    
        docCallout : Optional Doc2 -> Doc2 -> Doc2
    
        docBlockquote : Doc2 -> Doc2
    
        docTooltip : Doc2 -> Doc2 -> Doc2
    
        docTable : [[Doc2]] -> Doc2
    
    This is an aside. (
    Some extra detail that doesn't belong in main text. )
    
      | This is an important callout, with no icon.
    
      | 🌻
      | 
      | This is an important callout, with an icon. The text
      | wraps onto multiple lines.
    
    > "And what is the use of a book," thought Alice, "without
    > pictures or conversation?"
    > 
    > *Lewis Carroll, Alice's Adventures in Wonderland*
    
    Hover over me
    
    a           b           A longer paragraph that will split
                            onto multiple lines, such that this
                            row occupies multiple lines in the
                            rendered table.
    Some text   More text   Zounds!
````

But we can't display this due to a decompilation problem.

``` unison
rendered = Pretty.get (docFormatConsole doc.guide)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + rendered : Annotated () (Either SpecialForm ConsoleText)

  Run `update` to apply these changes to your codebase.
```

```` ucm
> display rendered

  # Unison computable documentation

    # Basic formatting
    
      Paragraphs are separated by one or more blanklines.
      Sections have a title and 0 or more paragraphs or other
      section elements.
    
      Text can be bold, *italicized*, ~~strikethrough~~, or
      `monospaced` (or `monospaced`).
    
      You can link to Unison terms, types, and external URLs:
    
      * An external url
      * Some is a term link; Optional is a type link
      * A named type link and a named term link. Term links are
        handy for linking to other documents!
    
      You can use `{{ .. }}` to escape out to regular Unison
      syntax, for instance __not bold__. This is useful for
      creating documents programmatically or just including
      other documents.
    
      *Next up:* lists

    # Lists
    
      # Bulleted lists
      
        Bulleted lists can use `+`, `-`, or `*` for the bullets
        (though the choice will be normalized away by the
        pretty-printer). They can be nested, to any depth:
      
        * A
        * B
        * C
          * C1
          * C2
    
      # Numbered lists
      
        1. A
        2. B
        3. C
      
        The first number of the list determines the starting
        number in the rendered output. The other numbers are
        ignored:
      
        10. A
        11. B
        12. C
      
        Numbered lists can be nested as well, and combined with
        bulleted lists:
      
        1. Wake up.
           * What am I doing here?
           * In this nested list.
        2. Take shower.
        3. Get dressed.

    # Evaluation
    
      Expressions can be evaluated inline, for instance `2`.
    
      Blocks of code can be evaluated as well, for instance:
    
          id x = x
          id (sqr 10)
          ⧨
          100
    
      also:
    
          match 1 with
            1 -> "hi"
            _ -> "goodbye"
          ⧨
          "hi"
    
      To include a typechecked snippet of code without
      evaluating it, you can do:
    
          use Nat *
          cube : Nat -> Nat
          cube x = x * x * x

    # Including Unison source code
    
      Unison definitions can be included in docs. For instance:
    
          structural type Optional a = None | Some a
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      Some rendering targets also support folded source:
    
          structural type Optional a = None | Some a
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      You can also include just a signature, inline, with
      `sqr : Nat -> Nat`, or you can include one or more
      signatures as a block:
    
          sqr : Nat -> Nat
      
          Nat.+ : Nat -> Nat -> Nat
    
      Or alternately:
    
          List.map : (a ->{e} b) -> [a] ->{e} [b]
    
      # Inline snippets
      
        You can include typechecked code snippets inline, for
        instance:
      
        * `f x Nat.+ sqr 1` - the `2` says to ignore the first
          two arguments when rendering. In richer renderers, the
          `sqr` link will be clickable.
        * If your snippet expression is just a single function
          application, you can put it in double backticks, like
          so: `sqr x`. This is equivalent to `sqr x`.

    # Non-Unison code blocks
    
      Use three or more single quotes to start a block with no
      syntax highlighting:
    
      ``` raw
         _____     _             
        |  |  |___|_|___ ___ ___ 
        |  |  |   | |_ -| . |   |
        |_____|_|_|_|___|___|_|_|
        
      ```
    
      You can use three or more backticks plus a language name
      for blocks with syntax highlighting:
    
      ``` Haskell
      -- A fenced code block which isn't parsed by Unison
      reverse = foldl (flip (:)) []
      ```
    
      ``` Scala
      // A fenced code block which isn't parsed by Unison
      def reverse[A](xs: List[A]) = 
        xs.foldLeft(Nil : List[A])((acc,a) => a +: acc)
      ```

    There are also asides, callouts, tables, tooltips, and more.
    These don't currently have special syntax; just use the
    `{{ }}` syntax to call these functions directly.
    
        docAside : Doc2 -> Doc2
    
        docCallout : Optional Doc2 -> Doc2 -> Doc2
    
        docBlockquote : Doc2 -> Doc2
    
        docTooltip : Doc2 -> Doc2 -> Doc2
    
        docTable : [[Doc2]] -> Doc2
    
    This is an aside. (
    Some extra detail that doesn't belong in main text. )
    
      | This is an important callout, with no icon.
    
      | 🌻
      | 
      | This is an important callout, with an icon. The text
      | wraps onto multiple lines.
    
    > "And what is the use of a book," thought Alice, "without
    > pictures or conversation?"
    > 
    > *Lewis Carroll, Alice's Adventures in Wonderland*
    
    Hover over me
    
    a           b           A longer paragraph that will split
                            onto multiple lines, such that this
                            row occupies multiple lines in the
                            rendered table.
    Some text   More text   Zounds!

> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> display rendered

  # Unison computable documentation

    # Basic formatting
    
      Paragraphs are separated by one or more blanklines.
      Sections have a title and 0 or more paragraphs or other
      section elements.
    
      Text can be bold, *italicized*, ~~strikethrough~~, or
      `monospaced` (or `monospaced`).
    
      You can link to Unison terms, types, and external URLs:
    
      * An external url
      * Some is a term link; Optional is a type link
      * A named type link and a named term link. Term links are
        handy for linking to other documents!
    
      You can use `{{ .. }}` to escape out to regular Unison
      syntax, for instance __not bold__. This is useful for
      creating documents programmatically or just including
      other documents.
    
      *Next up:* lists

    # Lists
    
      # Bulleted lists
      
        Bulleted lists can use `+`, `-`, or `*` for the bullets
        (though the choice will be normalized away by the
        pretty-printer). They can be nested, to any depth:
      
        * A
        * B
        * C
          * C1
          * C2
    
      # Numbered lists
      
        1. A
        2. B
        3. C
      
        The first number of the list determines the starting
        number in the rendered output. The other numbers are
        ignored:
      
        10. A
        11. B
        12. C
      
        Numbered lists can be nested as well, and combined with
        bulleted lists:
      
        1. Wake up.
           * What am I doing here?
           * In this nested list.
        2. Take shower.
        3. Get dressed.

    # Evaluation
    
      Expressions can be evaluated inline, for instance `2`.
    
      Blocks of code can be evaluated as well, for instance:
    
          id x = x
          id (sqr 10)
          ⧨
          100
    
      also:
    
          match 1 with
            1 -> "hi"
            _ -> "goodbye"
          ⧨
          "hi"
    
      To include a typechecked snippet of code without
      evaluating it, you can do:
    
          use Nat *
          cube : Nat -> Nat
          cube x = x * x * x

    # Including Unison source code
    
      Unison definitions can be included in docs. For instance:
    
          structural type Optional a = None | Some a
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      Some rendering targets also support folded source:
    
          structural type Optional a = None | Some a
          
          sqr : Nat -> Nat
          sqr x =
            use Nat *
            x * x
    
      You can also include just a signature, inline, with
      `sqr : Nat -> Nat`, or you can include one or more
      signatures as a block:
    
          sqr : Nat -> Nat
      
          Nat.+ : Nat -> Nat -> Nat
    
      Or alternately:
    
          List.map : (a ->{e} b) -> [a] ->{e} [b]
    
      # Inline snippets
      
        You can include typechecked code snippets inline, for
        instance:
      
        * `f x Nat.+ sqr 1` - the `2` says to ignore the first
          two arguments when rendering. In richer renderers, the
          `sqr` link will be clickable.
        * If your snippet expression is just a single function
          application, you can put it in double backticks, like
          so: `sqr x`. This is equivalent to `sqr x`.

    # Non-Unison code blocks
    
      Use three or more single quotes to start a block with no
      syntax highlighting:
    
      ``` raw
         _____     _             
        |  |  |___|_|___ ___ ___ 
        |  |  |   | |_ -| . |   |
        |_____|_|_|_|___|___|_|_|
        
      ```
    
      You can use three or more backticks plus a language name
      for blocks with syntax highlighting:
    
      ``` Haskell
      -- A fenced code block which isn't parsed by Unison
      reverse = foldl (flip (:)) []
      ```
    
      ``` Scala
      // A fenced code block which isn't parsed by Unison
      def reverse[A](xs: List[A]) = 
        xs.foldLeft(Nil : List[A])((acc,a) => a +: acc)
      ```

    There are also asides, callouts, tables, tooltips, and more.
    These don't currently have special syntax; just use the
    `{{ }}` syntax to call these functions directly.
    
        docAside : Doc2 -> Doc2
    
        docCallout : Optional Doc2 -> Doc2 -> Doc2
    
        docBlockquote : Doc2 -> Doc2
    
        docTooltip : Doc2 -> Doc2 -> Doc2
    
        docTable : [[Doc2]] -> Doc2
    
    This is an aside. (
    Some extra detail that doesn't belong in main text. )
    
      | This is an important callout, with no icon.
    
      | 🌻
      | 
      | This is an important callout, with an icon. The text
      | wraps onto multiple lines.
    
    > "And what is the use of a book," thought Alice, "without
    > pictures or conversation?"
    > 
    > *Lewis Carroll, Alice's Adventures in Wonderland*
    
    Hover over me
    
    a           b           A longer paragraph that will split
                            onto multiple lines, such that this
                            row occupies multiple lines in the
                            rendered table.
    Some text   More text   Zounds!

> undo

  Here are the changes I undid

  Added definitions:

    1. rendered : Annotated () (Either SpecialForm ConsoleText)
````

And then this sometimes generates a GHC crash "strange closure error" but doesn't seem deterministic.

``` unison
rendered = Pretty.get (docFormatConsole doc.guide)

> rendered
```

```` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + rendered : Annotated () (Either SpecialForm ConsoleText)

  Run `update` to apply these changes to your codebase.

    3 | > rendered
          ⧩
          Annotated.Group
            ()
            (Annotated.Append
              ()
              [ Indent
                  ()
                  (Annotated.Lit () (Right (Plain "# ")))
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Append
                        ()
                        [ Annotated.Lit
                            ()
                            (Right
                              (ConsoleText.Bold (Plain "Unison")))
                        , Annotated.Lit
                            ()
                            (Right
                              (ConsoleText.Bold
                                (Plain "computable")))
                        , Annotated.Lit
                            ()
                            (Right
                              (ConsoleText.Bold
                                (Plain "documentation")))
                        ])))
              , Annotated.Lit () (Right (Plain "\n"))
              , Annotated.Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "# ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "Basic")))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "formatting")))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "Paragraphs"))
                                    , Annotated.Lit
                                        () (Right (Plain "are"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "separated"))
                                    , Annotated.Lit
                                        () (Right (Plain "by"))
                                    , Annotated.Lit
                                        () (Right (Plain "one"))
                                    , Annotated.Lit
                                        () (Right (Plain "or"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "more"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "blanklines."))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "Sections"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "have"))
                                    , Annotated.Lit
                                        () (Right (Plain "a"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "title"))
                                    , Annotated.Lit
                                        () (Right (Plain "and"))
                                    , Annotated.Lit
                                        () (Right (Plain "0"))
                                    , Annotated.Lit
                                        () (Right (Plain "or"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "more"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "paragraphs"))
                                    , Annotated.Lit
                                        () (Right (Plain "or"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "other"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "section"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "elements."))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right (Plain "Text"))
                                    , Annotated.Lit
                                        () (Right (Plain "can"))
                                    , Annotated.Lit
                                        () (Right (Plain "be"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Wrap
                                              ()
                                              (Annotated.Lit
                                                ()
                                                (Right
                                                  (ConsoleText.Bold
                                                    (Plain
                                                      "bold"))))
                                          , Annotated.Lit
                                              ()
                                              (Right (Plain ","))
                                          ])
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Group
                                              ()
                                              (Annotated.Append
                                                ()
                                                [ Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain "*"))
                                                , Wrap
                                                    ()
                                                    (Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "italicized")))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain "*"))
                                                ])
                                          , Annotated.Lit
                                              ()
                                              (Right (Plain ","))
                                          ])
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Group
                                              ()
                                              (Annotated.Append
                                                ()
                                                [ Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "~~"))
                                                , Wrap
                                                    ()
                                                    (Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "strikethrough")))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "~~"))
                                                ])
                                          , Annotated.Lit
                                              ()
                                              (Right (Plain ","))
                                          ])
                                    , Annotated.Lit
                                        () (Right (Plain "or"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Lit
                                              ()
                                              (Right (Plain "`"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "monospaced"))
                                          , Annotated.Lit
                                              ()
                                              (Right (Plain "`"))
                                          ])
                                    , Annotated.Lit
                                        () (Right (Plain "(or"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Group
                                              ()
                                              (Annotated.Append
                                                ()
                                                [ Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain "`"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "monospaced"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain "`"))
                                                ])
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain ")."))
                                          ])
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        () (Right (Plain "You"))
                                    , Annotated.Lit
                                        () (Right (Plain "can"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "link"))
                                    , Annotated.Lit
                                        () (Right (Plain "to"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "Unison"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "terms,"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "types,"))
                                    , Annotated.Lit
                                        () (Right (Plain "and"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "external"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "URLs:"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "* ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Wrap
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Underline
                                                      (Plain
                                                        "An")))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Underline
                                                      (Plain
                                                        "external")))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Underline
                                                      (Plain
                                                        "url")))
                                              ])))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "* ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Wrap
                                          ()
                                          (Annotated.Append
                                            ()
                                            [ Annotated.Lit
                                                ()
                                                (Left
                                                  (SpecialForm.Link
                                                    (Right
                                                      (Doc2.Term.Term
                                                        (Any
                                                          (do
                                                            Some))))))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "is"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "a"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "term"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "link;"))
                                            , Annotated.Lit
                                                ()
                                                (Left
                                                  (SpecialForm.Link
                                                    (Left
                                                      (typeLink Optional))))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "is"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "a"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "type"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "link"))
                                            ]))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "* ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Wrap
                                          ()
                                          (Annotated.Append
                                            ()
                                            [ Wrap
                                                ()
                                                (Annotated.Append
                                                  ()
                                                  [ Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Underline
                                                          (Plain
                                                            "A")))
                                                  , Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Underline
                                                          (Plain
                                                            "named")))
                                                  , Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Underline
                                                          (Plain
                                                            "type")))
                                                  , Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Underline
                                                          (Plain
                                                            "link")))
                                                  ])
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "and"))
                                            , Annotated.Group
                                                ()
                                                (Annotated.Append
                                                  ()
                                                  [ Wrap
                                                      ()
                                                      (Annotated.Append
                                                        ()
                                                        [ Annotated.Lit
                                                            ()
                                                            (Right
                                                              (Underline
                                                                (Plain
                                                                  "a")))
                                                        , Annotated.Lit
                                                            ()
                                                            (Right
                                                              (Underline
                                                                (Plain
                                                                  "named")))
                                                        , Annotated.Lit
                                                            ()
                                                            (Right
                                                              (Underline
                                                                (Plain
                                                                  "term")))
                                                        , Annotated.Lit
                                                            ()
                                                            (Right
                                                              (Underline
                                                                (Plain
                                                                  "link")))
                                                        ])
                                                  , Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "."))
                                                  ])
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "Term"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "links"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "are"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "handy"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "for"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "linking"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "to"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "other"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "documents!"))
                                            ]))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        () (Right (Plain "You"))
                                    , Annotated.Lit
                                        () (Right (Plain "can"))
                                    , Annotated.Lit
                                        () (Right (Plain "use"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Lit
                                              ()
                                              (Right (Plain "`"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "{{ .. }}"))
                                          , Annotated.Lit
                                              ()
                                              (Right (Plain "`"))
                                          ])
                                    , Annotated.Lit
                                        () (Right (Plain "to"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "escape"))
                                    , Annotated.Lit
                                        () (Right (Plain "out"))
                                    , Annotated.Lit
                                        () (Right (Plain "to"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "regular"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "Unison"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "syntax,"))
                                    , Annotated.Lit
                                        () (Right (Plain "for"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "instance"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "__not bold__"))
                                          , Annotated.Lit
                                              ()
                                              (Right (Plain "."))
                                          ])
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "This"))
                                    , Annotated.Lit
                                        () (Right (Plain "is"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "useful"))
                                    , Annotated.Lit
                                        () (Right (Plain "for"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "creating"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "documents"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain
                                            "programmatically"))
                                    , Annotated.Lit
                                        () (Right (Plain "or"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "just"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "including"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "other"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "documents."))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Lit
                                              ()
                                              (Right (Plain "*"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "Next"))
                                          ])
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "up:"))
                                          , Annotated.Lit
                                              ()
                                              (Right (Plain "*"))
                                          ])
                                    , Annotated.Lit
                                        ()
                                        (Left
                                          (SpecialForm.Link
                                            (Right
                                              (Doc2.Term.Term
                                                (Any (do lists))))))
                                    ])))
                          ]))))
              , Annotated.Lit () (Right (Plain "\n"))
              , Annotated.Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "# ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Lit
                                    ()
                                    (Right
                                      (ConsoleText.Bold
                                        (Plain "Lists"))))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "# ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Annotated.Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "Bulleted")))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "lists")))
                                              ])))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "Bulleted"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "lists"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "can"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "use"))
                                              , Annotated.Group
                                                  ()
                                                  (Annotated.Append
                                                    ()
                                                    [ Annotated.Group
                                                        ()
                                                        (Annotated.Append
                                                          ()
                                                          [ Annotated.Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "`"))
                                                          , Annotated.Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "+"))
                                                          , Annotated.Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "`"))
                                                          ])
                                                    , Annotated.Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            ","))
                                                    ])
                                              , Annotated.Group
                                                  ()
                                                  (Annotated.Append
                                                    ()
                                                    [ Annotated.Group
                                                        ()
                                                        (Annotated.Append
                                                          ()
                                                          [ Annotated.Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "`"))
                                                          , Annotated.Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "-"))
                                                          , Annotated.Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "`"))
                                                          ])
                                                    , Annotated.Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            ","))
                                                    ])
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "or"))
                                              , Annotated.Group
                                                  ()
                                                  (Annotated.Append
                                                    ()
                                                    [ Annotated.Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            "`"))
                                                    , Annotated.Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            "*"))
                                                    , Annotated.Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            "`"))
                                                    ])
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "for"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "bullets"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "(though"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "choice"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "will"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "be"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "normalized"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "away"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "by"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "pretty-printer)."))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "They"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "can"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "be"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "nested,"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "to"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "any"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "depth:"))
                                              ])))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Annotated.Group
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "* ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "  ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "A"))))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "* ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "  ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "B"))))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "* ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "  ")))
                                                  (Annotated.Append
                                                    ()
                                                    [ Wrap
                                                        ()
                                                        (Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "C")))
                                                    , Annotated.Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            "\n"))
                                                    , Annotated.Group
                                                        ()
                                                        (Annotated.Append
                                                          ()
                                                          [ Indent
                                                              ()
                                                              (Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "* ")))
                                                              (Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "  ")))
                                                              (Wrap
                                                                ( 
                                                                )
                                                                (Annotated.Lit
                                                                  ( 
                                                                  )
                                                                  (Right
                                                                    (Plain
                                                                      "C1"))))
                                                          , Annotated.Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "\n"))
                                                          , Indent
                                                              ()
                                                              (Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "* ")))
                                                              (Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "  ")))
                                                              (Wrap
                                                                ( 
                                                                )
                                                                (Annotated.Lit
                                                                  ( 
                                                                  )
                                                                  (Right
                                                                    (Plain
                                                                      "C2"))))
                                                          ])
                                                    ])
                                              ])))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "# ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Annotated.Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "Numbered")))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "lists")))
                                              ])))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Annotated.Group
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "1. ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "A"))))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "2. ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "B"))))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "3. ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "C"))))
                                              ])))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "The"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "first"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "number"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "of"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "list"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "determines"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "starting"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "number"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "in"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "the"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "rendered"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "output."))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "The"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "other"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "numbers"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "are"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "ignored:"))
                                              ])))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Annotated.Group
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "10. ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "    ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "A"))))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "11. ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "    ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "B"))))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "12. ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "    ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Lit
                                                      ()
                                                      (Right
                                                        (Plain
                                                          "C"))))
                                              ])))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "Numbered"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "lists"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "can"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "be"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "nested"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "as"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "well,"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "and"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "combined"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "with"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "bulleted"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "lists:"))
                                              ])))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Annotated.Group
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "1. ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Annotated.Append
                                                    ()
                                                    [ Wrap
                                                        ()
                                                        (Annotated.Append
                                                          ()
                                                          [ Annotated.Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "Wake"))
                                                          , Annotated.Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "up."))
                                                          ])
                                                    , Annotated.Lit
                                                        ()
                                                        (Right
                                                          (Plain
                                                            "\n"))
                                                    , Annotated.Group
                                                        ()
                                                        (Annotated.Append
                                                          ()
                                                          [ Indent
                                                              ()
                                                              (Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "* ")))
                                                              (Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "  ")))
                                                              (Wrap
                                                                ( 
                                                                )
                                                                (Annotated.Append
                                                                  ( 
                                                                  )
                                                                  [ Annotated.Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "What"))
                                                                  , Annotated.Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "am"))
                                                                  , Annotated.Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "I"))
                                                                  , Annotated.Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "doing"))
                                                                  , Annotated.Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "here?"))
                                                                  ]))
                                                          , Annotated.Lit
                                                              ()
                                                              (Right
                                                                (Plain
                                                                  "\n"))
                                                          , Indent
                                                              ()
                                                              (Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "* ")))
                                                              (Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "  ")))
                                                              (Wrap
                                                                ( 
                                                                )
                                                                (Annotated.Append
                                                                  ( 
                                                                  )
                                                                  [ Annotated.Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "In"))
                                                                  , Annotated.Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "this"))
                                                                  , Annotated.Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "nested"))
                                                                  , Annotated.Lit
                                                                      ( 
                                                                      )
                                                                      (Right
                                                                        (Plain
                                                                          "list."))
                                                                  ]))
                                                          ])
                                                    ])
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "2. ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "Take"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "shower."))
                                                      ]))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "3. ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "   ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "Get"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "dressed."))
                                                      ]))
                                              ])))
                                    ])))
                          ]))))
              , Annotated.Lit () (Right (Plain "\n"))
              , Annotated.Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "# ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Lit
                                    ()
                                    (Right
                                      (ConsoleText.Bold
                                        (Plain "Evaluation"))))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "Expressions"))
                                    , Annotated.Lit
                                        () (Right (Plain "can"))
                                    , Annotated.Lit
                                        () (Right (Plain "be"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "evaluated"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "inline,"))
                                    , Annotated.Lit
                                        () (Right (Plain "for"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "instance"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Lit
                                              ()
                                              (Left
                                                (EvalInline
                                                  (Doc2.Term.Term
                                                    (Any
                                                      (do
                                                        1
                                                          Nat.+ 1)))))
                                          , Annotated.Lit
                                              ()
                                              (Right (Plain "."))
                                          ])
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right (Plain "Blocks"))
                                    , Annotated.Lit
                                        () (Right (Plain "of"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "code"))
                                    , Annotated.Lit
                                        () (Right (Plain "can"))
                                    , Annotated.Lit
                                        () (Right (Plain "be"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "evaluated"))
                                    , Annotated.Lit
                                        () (Right (Plain "as"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "well,"))
                                    , Annotated.Lit
                                        () (Right (Plain "for"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "instance:"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                () (Annotated.Lit
                                  () (Left
                                    (Eval
                                      (Doc2.Term.Term
                                        (Any
                                          (do
                                            id x = x
                                            id (sqr 10))))))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Lit
                                    () (Right (Plain "also:")))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                () (Annotated.Lit
                                  () (Left
                                    (Eval
                                      (Doc2.Term.Term
                                        (Any
                                          (do match 1 with
                                            1 -> "hi"
                                            _ -> "goodbye")))))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        () (Right (Plain "To"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "include"))
                                    , Annotated.Lit
                                        () (Right (Plain "a"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "typechecked"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "snippet"))
                                    , Annotated.Lit
                                        () (Right (Plain "of"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "code"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "without"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "evaluating"))
                                    , Annotated.Lit
                                        () (Right (Plain "it,"))
                                    , Annotated.Lit
                                        () (Right (Plain "you"))
                                    , Annotated.Lit
                                        () (Right (Plain "can"))
                                    , Annotated.Lit
                                        () (Right (Plain "do:"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                () (Annotated.Lit
                                  () (Left
                                    (ExampleBlock
                                      0 (Doc2.Term.Term
                                        (Any
                                          (do
                                            use Nat *
                                            cube : Nat -> Nat
                                            cube x = x * x * x
                                            ())))))))
                          ]))))
              , Annotated.Lit () (Right (Plain "\n"))
              , Annotated.Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "# ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "Including")))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "Unison")))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "source")))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "code")))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right (Plain "Unison"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "definitions"))
                                    , Annotated.Lit
                                        () (Right (Plain "can"))
                                    , Annotated.Lit
                                        () (Right (Plain "be"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "included"))
                                    , Annotated.Lit
                                        () (Right (Plain "in"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "docs."))
                                    , Annotated.Lit
                                        () (Right (Plain "For"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "instance:"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Lit
                                    ()
                                    (Left
                                      (SpecialForm.Source
                                        [ ( Left
                                              (typeLink Optional)
                                          , []
                                          )
                                        , ( Right
                                              (Doc2.Term.Term
                                                (Any (do sqr)))
                                          , []
                                          )
                                        ])))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right (Plain "Some"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "rendering"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "targets"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "also"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "support"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "folded"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "source:"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Lit
                                    ()
                                    (Left
                                      (FoldedSource
                                        [ ( Left
                                              (typeLink Optional)
                                          , []
                                          )
                                        , ( Right
                                              (Doc2.Term.Term
                                                (Any (do sqr)))
                                          , []
                                          )
                                        ])))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        () (Right (Plain "You"))
                                    , Annotated.Lit
                                        () (Right (Plain "can"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "also"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "include"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "just"))
                                    , Annotated.Lit
                                        () (Right (Plain "a"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "signature,"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "inline,"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "with"))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Lit
                                              ()
                                              (Left
                                                (SignatureInline
                                                  (Doc2.Term.Term
                                                    (Any
                                                      (do sqr)))))
                                          , Annotated.Lit
                                              ()
                                              (Right (Plain ","))
                                          ])
                                    , Annotated.Lit
                                        () (Right (Plain "or"))
                                    , Annotated.Lit
                                        () (Right (Plain "you"))
                                    , Annotated.Lit
                                        () (Right (Plain "can"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "include"))
                                    , Annotated.Lit
                                        () (Right (Plain "one"))
                                    , Annotated.Lit
                                        () (Right (Plain "or"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "more"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "signatures"))
                                    , Annotated.Lit
                                        () (Right (Plain "as"))
                                    , Annotated.Lit
                                        () (Right (Plain "a"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "block:"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Lit
                                    ()
                                    (Left
                                      (SpecialForm.Signature
                                        [ Doc2.Term.Term
                                            (Any (do sqr))
                                        , Doc2.Term.Term
                                            (Any (do (Nat.+)))
                                        ])))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        () (Right (Plain "Or"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "alternately:"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Lit
                                    ()
                                    (Left
                                      (SpecialForm.Signature
                                        [ Doc2.Term.Term
                                            (Any (do List.map))
                                        ])))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "# ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Annotated.Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "Inline")))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (ConsoleText.Bold
                                                      (Plain
                                                        "snippets")))
                                              ])))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Wrap
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "You"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "can"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "include"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "typechecked"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "code"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "snippets"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "inline,"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "for"))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain
                                                      "instance:"))
                                              ])))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Indent
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "  ")))
                                        (Annotated.Group
                                          ()
                                          (Annotated.Group
                                            ()
                                            (Annotated.Append
                                              ()
                                              [ Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "* ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "  ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Annotated.Lit
                                                          ()
                                                          (Left
                                                            (Example
                                                              2
                                                              (Doc2.Term.Term
                                                                (Any
                                                                  (do
                                                                    f
                                                                    x ->
                                                                      f
                                                                        x
                                                                        Nat.+ sqr
                                                                          1)))))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "-"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "the"))
                                                      , Annotated.Group
                                                          ()
                                                          (Annotated.Append
                                                            ()
                                                            [ Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "`"))
                                                            , Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "2"))
                                                            , Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "`"))
                                                            ])
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "says"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "to"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "ignore"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "the"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "first"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "two"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "arguments"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "when"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "rendering."))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "In"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "richer"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "renderers,"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "the"))
                                                      , Annotated.Group
                                                          ()
                                                          (Annotated.Append
                                                            ()
                                                            [ Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "`"))
                                                            , Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "sqr"))
                                                            , Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "`"))
                                                            ])
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "link"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "will"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "be"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "clickable."))
                                                      ]))
                                              , Annotated.Lit
                                                  ()
                                                  (Right
                                                    (Plain "\n"))
                                              , Indent
                                                  ()
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "* ")))
                                                  (Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "  ")))
                                                  (Wrap
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "If"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "your"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "snippet"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "expression"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "is"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "just"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "a"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "single"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "function"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "application,"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "you"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "can"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "put"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "it"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "in"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "double"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "backticks,"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "like"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "so:"))
                                                      , Annotated.Group
                                                          ()
                                                          (Annotated.Append
                                                            ()
                                                            [ Annotated.Lit
                                                                ( 
                                                                )
                                                                (Left
                                                                  (Example
                                                                    1
                                                                    (Doc2.Term.Term
                                                                      (Any
                                                                        (do
                                                                          x ->
                                                                            sqr
                                                                              x)))))
                                                            , Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "."))
                                                            ])
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "This"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "is"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "equivalent"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "to"))
                                                      , Annotated.Group
                                                          ()
                                                          (Annotated.Append
                                                            ()
                                                            [ Annotated.Lit
                                                                ( 
                                                                )
                                                                (Left
                                                                  (Example
                                                                    1
                                                                    (Doc2.Term.Term
                                                                      (Any
                                                                        (do
                                                                          x ->
                                                                            sqr
                                                                              x)))))
                                                            , Annotated.Lit
                                                                ( 
                                                                )
                                                                (Right
                                                                  (Plain
                                                                    "."))
                                                            ])
                                                      ]))
                                              ])))
                                    ])))
                          ]))))
              , Annotated.Lit () (Right (Plain "\n"))
              , Annotated.Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "# ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "Non-Unison")))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "code")))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (ConsoleText.Bold
                                            (Plain "blocks")))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        () (Right (Plain "Use"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "three"))
                                    , Annotated.Lit
                                        () (Right (Plain "or"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "more"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "single"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "quotes"))
                                    , Annotated.Lit
                                        () (Right (Plain "to"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "start"))
                                    , Annotated.Lit
                                        () (Right (Plain "a"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "block"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "with"))
                                    , Annotated.Lit
                                        () (Right (Plain "no"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "syntax"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "highlighting:"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Group
                                    ()
                                    (Annotated.Append
                                      ()
                                      [ Annotated.Lit
                                          ()
                                          (Right (Plain "``` "))
                                      , Annotated.Group
                                          ()
                                          (Annotated.Lit
                                            ()
                                            (Right (Plain "raw")))
                                      , Annotated.Lit
                                          ()
                                          (Right (Plain "\n"))
                                      , Annotated.Lit
                                          ()
                                          (Right
                                            (Plain
                                              "   _____     _             \n  |  |  |___|_|___ ___ ___ \n  |  |  |   | |_ -| . |   |\n  |_____|_|_|_|___|___|_|_|\n  "))
                                      , Annotated.Lit
                                          ()
                                          (Right (Plain "\n"))
                                      , Annotated.Lit
                                          ()
                                          (Right (Plain "```"))
                                      ]))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        () (Right (Plain "You"))
                                    , Annotated.Lit
                                        () (Right (Plain "can"))
                                    , Annotated.Lit
                                        () (Right (Plain "use"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "three"))
                                    , Annotated.Lit
                                        () (Right (Plain "or"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "more"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "backticks"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "plus"))
                                    , Annotated.Lit
                                        () (Right (Plain "a"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "language"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "name"))
                                    , Annotated.Lit
                                        () (Right (Plain "for"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "blocks"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "with"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "syntax"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain "highlighting:"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right (Plain "``` "))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right
                                            (Plain "Haskell")))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain
                                            "-- A fenced code block which isn't parsed by Unison\nreverse = foldl (flip (:)) []"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "```"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Indent
                              ()
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Lit
                                () (Right (Plain "  ")))
                              (Annotated.Group
                                ()
                                (Annotated.Group
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right (Plain "``` "))
                                    , Annotated.Group
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right (Plain "Scala")))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        ()
                                        (Right
                                          (Plain
                                            "// A fenced code block which isn't parsed by Unison\ndef reverse[A](xs: List[A]) = \n  xs.foldLeft(Nil : List[A])((acc,a) => a +: acc)"))
                                    , Annotated.Lit
                                        () (Right (Plain "\n"))
                                    , Annotated.Lit
                                        () (Right (Plain "```"))
                                    ])))
                          ]))))
              , Annotated.Lit () (Right (Plain "\n"))
              , Annotated.Lit () (Right (Plain "\n"))
              , Indent
                  ()
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Lit () (Right (Plain "  ")))
                  (Annotated.Group
                    ()
                    (Wrap
                      ()
                      (Annotated.Group
                        ()
                        (Annotated.Append
                          ()
                          [ Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Append
                                  ()
                                  [ Annotated.Lit
                                      () (Right (Plain "There"))
                                  , Annotated.Lit
                                      () (Right (Plain "are"))
                                  , Annotated.Lit
                                      () (Right (Plain "also"))
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "asides,"))
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "callouts,"))
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "tables,"))
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "tooltips,"))
                                  , Annotated.Lit
                                      () (Right (Plain "and"))
                                  , Annotated.Lit
                                      () (Right (Plain "more."))
                                  , Annotated.Lit
                                      () (Right (Plain "These"))
                                  , Annotated.Lit
                                      () (Right (Plain "don't"))
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "currently"))
                                  , Annotated.Lit
                                      () (Right (Plain "have"))
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "special"))
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "syntax;"))
                                  , Annotated.Lit
                                      () (Right (Plain "just"))
                                  , Annotated.Lit
                                      () (Right (Plain "use"))
                                  , Annotated.Lit
                                      () (Right (Plain "the"))
                                  , Annotated.Group
                                      ()
                                      (Annotated.Append
                                        ()
                                        [ Annotated.Lit
                                            ()
                                            (Right (Plain "`"))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Plain "{{ }}"))
                                        , Annotated.Lit
                                            ()
                                            (Right (Plain "`"))
                                        ])
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "syntax"))
                                  , Annotated.Lit
                                      () (Right (Plain "to"))
                                  , Annotated.Lit
                                      () (Right (Plain "call"))
                                  , Annotated.Lit
                                      () (Right (Plain "these"))
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "functions"))
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "directly."))
                                  ]))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Lit
                                  ()
                                  (Left
                                    (SpecialForm.Signature
                                      [ Doc2.Term.Term
                                          (Any (do docAside))
                                      , Doc2.Term.Term
                                          (Any (do docCallout))
                                      , Doc2.Term.Term
                                          (Any
                                            (do docBlockquote))
                                      , Doc2.Term.Term
                                          (Any (do docTooltip))
                                      , Doc2.Term.Term
                                          (Any (do docTable))
                                      ]))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Append
                                  ()
                                  [ Annotated.Lit
                                      () (Right (Plain "This"))
                                  , Annotated.Lit
                                      () (Right (Plain "is"))
                                  , Annotated.Lit
                                      () (Right (Plain "an"))
                                  , Annotated.Lit
                                      ()
                                      (Right (Plain "aside."))
                                  , Annotated.Lit
                                      ()
                                      (Right
                                        (Foreground
                                          BrightBlack
                                          (Plain "(")))
                                  , Wrap
                                      ()
                                      (Annotated.Append
                                        ()
                                        [ Annotated.Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "Some")))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "extra")))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "detail")))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "that")))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "doesn't")))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "belong")))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "in")))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "main")))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Foreground
                                                BrightBlack
                                                (Plain "text.")))
                                        ])
                                  , Annotated.Lit
                                      ()
                                      (Right
                                        (Foreground
                                          BrightBlack
                                          (Plain ")")))
                                  ]))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Group
                                  ()
                                  (Indent
                                    ()
                                    (Annotated.Lit
                                      () (Right (Plain "  | ")))
                                    (Annotated.Lit
                                      () (Right (Plain "  | ")))
                                    (Wrap
                                      ()
                                      (Annotated.Append
                                        ()
                                        [ Annotated.Lit
                                            ()
                                            (Right
                                              (Plain "This"))
                                        , Annotated.Lit
                                            ()
                                            (Right (Plain "is"))
                                        , Annotated.Lit
                                            ()
                                            (Right (Plain "an"))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Plain "important"))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Plain "callout,"))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Plain "with"))
                                        , Annotated.Lit
                                            ()
                                            (Right (Plain "no"))
                                        , Annotated.Lit
                                            ()
                                            (Right
                                              (Plain "icon."))
                                        ])))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Group
                                  ()
                                  (Indent
                                    ()
                                    (Annotated.Lit
                                      () (Right (Plain "  | ")))
                                    (Annotated.Lit
                                      () (Right (Plain "  | ")))
                                    (Annotated.Append
                                      ()
                                      [ Wrap
                                          ()
                                          (Annotated.Lit
                                            ()
                                            (Right
                                              (ConsoleText.Bold
                                                (Plain "🌻"))))
                                      , Annotated.Lit
                                          ()
                                          (Right (Plain "\n"))
                                      , Annotated.Lit
                                          () (Right (Plain ""))
                                      , Annotated.Lit
                                          ()
                                          (Right (Plain "\n"))
                                      , Wrap
                                          ()
                                          (Annotated.Append
                                            ()
                                            [ Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "This"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "is"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "an"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "important"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "callout,"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "with"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "an"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "icon."))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "The"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "text"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "wraps"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain "onto"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "multiple"))
                                            , Annotated.Lit
                                                ()
                                                (Right
                                                  (Plain
                                                    "lines."))
                                            ])
                                      ]))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Group
                                  ()
                                  (Indent
                                    ()
                                    (Annotated.Lit
                                      () (Right (Plain "> ")))
                                    (Annotated.Lit
                                      () (Right (Plain "> ")))
                                    (Annotated.Group
                                      ()
                                      (Annotated.Append
                                        ()
                                        [ Annotated.Group
                                            ()
                                            (Wrap
                                              ()
                                              (Annotated.Append
                                                ()
                                                [ Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "\"And"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "what"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "is"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "the"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "use"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "of"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain "a"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "book,\""))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "thought"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "Alice,"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "\"without"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "pictures"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "or"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "conversation?\""))
                                                ]))
                                        , Annotated.Lit
                                            ()
                                            (Right (Plain "\n"))
                                        , Annotated.Lit
                                            ()
                                            (Right (Plain "\n"))
                                        , Annotated.Group
                                            ()
                                            (Wrap
                                              ()
                                              (Annotated.Append
                                                ()
                                                [ Annotated.Group
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "*"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "Lewis"))
                                                      ])
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "Carroll,"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "Alice's"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "Adventures"))
                                                , Annotated.Lit
                                                    ()
                                                    (Right
                                                      (Plain
                                                        "in"))
                                                , Annotated.Group
                                                    ()
                                                    (Annotated.Append
                                                      ()
                                                      [ Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "Wonderland"))
                                                      , Annotated.Lit
                                                          ()
                                                          (Right
                                                            (Plain
                                                              "*"))
                                                      ])
                                                ]))
                                        ])))))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Wrap
                                  ()
                                  (Annotated.Append
                                    ()
                                    [ Annotated.Lit
                                        ()
                                        (Right (Plain "Hover"))
                                    , Annotated.Lit
                                        ()
                                        (Right (Plain "over"))
                                    , Annotated.Lit
                                        () (Right (Plain "me"))
                                    ])))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Lit
                              () (Right (Plain "\n"))
                          , Annotated.Group
                              ()
                              (Wrap
                                ()
                                (Annotated.Table
                                  ()
                                  [ [ Wrap
                                        ()
                                        (Annotated.Lit
                                          () (Right (Plain "a")))
                                    , Wrap
                                        ()
                                        (Annotated.Lit
                                          () (Right (Plain "b")))
                                    , Wrap
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Lit
                                              ()
                                              (Right (Plain "A"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "longer"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "paragraph"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "that"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "will"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "split"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "onto"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "multiple"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "lines,"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "such"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "that"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "this"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "row"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "occupies"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "multiple"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "lines"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "in"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "the"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain
                                                  "rendered"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "table."))
                                          ])
                                    ]
                                  , [ Wrap
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "Some"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "text"))
                                          ])
                                    , Wrap
                                        ()
                                        (Annotated.Append
                                          ()
                                          [ Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "More"))
                                          , Annotated.Lit
                                              ()
                                              (Right
                                                (Plain "text"))
                                          ])
                                    , Wrap
                                        ()
                                        (Annotated.Lit
                                          ()
                                          (Right
                                            (Plain "Zounds!")))
                                    ]
                                  ]))
                          ]))))
              ])
````

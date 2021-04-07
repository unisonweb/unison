# Computable documents in Unison

Unison documentation is written in Unison and has some neat features:

* The documentation type provides a rich vocabulary of elements that go beyond markdown, including asides, callouts, tooltips, and more.
* Docs may contain Unison code which is parsed and typechecked to ensure validity. No more out of date examples that don't compile or assume a bunch of implicit context!
* Embeded examples are live and can show the results of evaluation. This uses the same evaluation cache as Unison's scratch files, allowing Unison docs to function like well-commented spreadsheets or notebooks.
* Docs links to other definitions are typechecked to ensure they point to valid defintions. The links are resolved to hashes and won't be broken by name changes or moving definitions around.
* Docs can be included in other docs and you can assemble documentation programmatically, using Unison code.
* There's a powerful textual syntax for all of the above, which we'll introduce next.

## Introduction

Documentation blocks start with `{{` and end with a matching `}}`. You can introduce doc blocks anywhere you'd use an expression, and you can also have anonymous documentation blocks immediately before a top-level term (and soon, types as well).

```unison
name = {{Alice}}
d1 = {{ Hello there {{name}}! }}

{{ An important constant, equal to @eval{ImportantConstant} }}
ImportantConstant = 41 + 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ImportantConstant     : Nat
      ImportantConstant.doc : Doc2
      d1                    : Doc2
      name                  : Doc2

```
Notice that an anonymous documentation block `{{ ... }}` before a definition `ImportantConstant` is just syntax sugar for `ImportantConstant.doc = {{ blah }}`.

You can preview what docs will look like when rendered to the console using the `display` or `docs` commands:

```ucm
.> display d1

  Hello there Alice!

.> docs ImportantConstant

  An important constant, equal to `42`

```
The `docs ImportantConstant` command will look for `ImportantConstant.doc` in the file or codebase. You can do this instead of explicitly linking docs to definitions.

## Syntax guide

```ucm
.> load ./unison-src/new-runtime-transcripts/doc.md.files/syntax.u

  I found and typechecked these definitions in
  ./unison-src/new-runtime-transcripts/doc.md.files/syntax.u. If
  you do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      basicFormatting : Doc2
      evaluation      : Doc2
      includingSource : Doc2
      lists           : Doc2
      sqr             : Nat -> Nat

.> add

  ⍟ I've added these definitions:
  
    basicFormatting : Doc2
    evaluation      : Doc2
    includingSource : Doc2
    lists           : Doc2
    sqr             : Nat -> Nat

.> view basicFormatting

  basicFormatting : Doc2
  basicFormatting =
    {{
    # Basic formatting
    
      Paragraphs are separated by one or more blanklines.
      
      Text can be __bold__, *italicized*, ~~strikethrough~~, or
      ''monospaced''.
      
      You can link to Unison terms, types, and external URLs:
      
      * [An external url](https://unisonweb.org)
      * {Some} is a term link; {type Optional} is a type link
      * [A named type link]({type Optional}) and
        [a named term link]({Some}). Term links are handy for
        linking to other documents!
      
      ## Escaping formatting
      
         If you have some inline text you want to leave unparsed
         and have it render in a monospace font, surround it in
         two single quotes, like so: ''__some bold text__''
         
         If you don't want the monospace rendering, you can use
         ''{{ .. }}'' to escape out to regular Unison syntax,
         for instance {{ docWord "__not bold__" }}.
      
      ## Sections and subsections
      
         Sections consist of section titles followed by zero or
         more paragraphs or other section elements (such as
         subsections). Sections can be empty.
    }}

.> display basicFormatting

  # Basic formatting
  
    Paragraphs are separated by one or more blanklines.
  
    Text can be bold, *italicized*, ~~strikethrough~~, or
    `monospaced`.
  
    You can link to Unison terms, types, and external URLs:
  
    * An external url
    * Some is a term link; Optional is a type link
    * A named type link and a named term link. Term links are
      handy for linking to other documents!
  
    # Escaping formatting
    
      If you have some inline text you want to leave unparsed
      and have it render in a monospace font, surround it in two
      single quotes, like so: `__some bold text__`
    
      If you don't want the monospace rendering, you can use
      `{{ .. }}` to escape out to regular Unison syntax, for
      instance __not bold__.
  
    # Sections and subsections
    
      Sections consist of section titles followed by zero or
      more paragraphs or other section elements (such as
      subsections). Sections can be empty.

.> view lists

  lists : Doc2
  lists =
    {{
    # Lists
    
      ## Bulleted lists
      
         Bulleted lists can use ''+'', ''-'', or ''*'' for the
         bullets. They can be nested, to any depth:
         
         * A
         * B
         * C
           * C1
           * C2
      
      ## Numbered lists
      
         1. A
         2. B
         3. C
         
         The first number of the list determines the starting
         number in the rendered output. The other numbers are
         ignored:
         
         Numbered lists can be nested as well, and combined with
         bulleted lists:
         
         1. Wake up.
            * What am I doing here?
            * In this nested list.
         2. Take shower.
         3. Get dressed.
    }}

.> display lists

  # Lists
  
    # Bulleted lists
    
      Bulleted lists can use `+`, `-`, or `*` for the bullets.
      They can be nested, to any depth:
    
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
    
      Numbered lists can be nested as well, and combined with
      bulleted lists:
    
      1. Wake up.
         * What am I doing here?
         * In this nested list.
      2. Take shower.
      3. Get dressed.

.> view evaluation

  evaluation : Doc2
  evaluation =
    use Nat +
    {{
    # Evaluation
    
      Expressions can be evaluated inline, for instance
      @eval{1 + 1}.
      
      Blocks of code can be evaluated as well, for instance:
      
      ```
      id x = x
      id (sqr 10)
      ```
    }}

.> display evaluation

  # Evaluation
  
    Expressions can be evaluated inline, for instance `2`.
  
    Blocks of code can be evaluated as well, for instance:
  
        id x = x
        id (sqr 10)
        ⧨
        100

.> view includingSource

  includingSource : Doc2
  includingSource =
    use Nat +
    {{
    # Including Unison source code
    
      Unison definitions can be included in docs. For instance:
      
      {{
      docSource
        [ docSourceElement
          (docEmbedTypeLink typeLink Optional) [],
          docSourceElement (docEmbedTermLink (_ -> sqr)) [] ] }}
      
      Some rendering targets (like HTML) also support folded
      source:
      
      {{
      docFoldedSource
        [ docSourceElement
          (docEmbedTypeLink typeLink Optional) [],
          docSourceElement (docEmbedTermLink (_ -> sqr)) [] ] }}
      
      You can also include just a signature, inline, with
      {{
      docSignatureInline (docEmbedSignatureLink (_ -> sqr))
      }}, or you can include one or more signatures as a block:
      
      {{
      docSignature
        [ docEmbedSignatureLink (_ -> sqr),
          docEmbedSignatureLink (_ -> Nat.pow) ] }}
      
      ## Inline snippets
      
         You can include typechecked code snippets inline, for
         instance:
         
         * {{ docExample 2 '(f x -> f x + sqr 1) }} - the ''2''
           says to ignore the first two arguments when
           rendering. In richer renderers, the ''sqr'' link will
           be clickable.
         * If your snippet expression is just a single function
           application, you can put it in double backticks, like
           so: ``x -> sqr x``. This is equivalent to
           {{ docExample 1 '(x -> sqr x) }}.
    }}

.> view Optional sqr

  type builtin.Optional a = None | Some a
  
  sqr : Nat -> Nat
  sqr x =
    use Nat *
    x * x

.> display includingSource

  # Including Unison source code
  
    Unison definitions can be included in docs. For instance:
  
        type Optional a = None | Some a
        
        sqr x =
          use Nat *
          x * x
  
    Some rendering targets (like HTML) also support folded
    source:
  
        type Optional a = None | Some a
        
        sqr x =
          use Nat *
          x * x
  
    You can also include just a signature, inline, with
    `sqr : Nat -> Nat`, or you can include one or more
    signatures as a block:
  
        sqr : Nat -> Nat
    
        Nat.pow : Nat -> Nat -> Nat
  
    # Inline snippets
    
      You can include typechecked code snippets inline, for
      instance:
    
      * `f x Nat.+ sqr 1` - the `2` says to ignore the first two
        arguments when rendering. In richer renderers, the `sqr`
        link will be clickable.
      * If your snippet expression is just a single function
        application, you can put it in double backticks, like
        so: `sqr x`. This is equivalent to `sqr x`.

```

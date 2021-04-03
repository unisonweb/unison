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

  Hello there Alice !

.> docs ImportantConstant

  An important constant, equal to 42

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
    
      doc.guide : Doc2

.> add

  ⍟ I've added these definitions:
  
    doc.guide : Doc2

.> display doc.guide

  # Unison documentation format
  
    # Basic formatting
    
      Paragraphs are separated by one or more blanklines.
      There's syntax for bold, italics, and strikethrough text:
    
      ``` raw _italics_ or *italics* **bold text** or __moar
      bold text__ ~~striken text~~ ''some code'' [The Unison
      website](https://unisonweb.org) A link to a term: {Some}.
      [A named link to the ''List'' type]({type List}). ```
    
      Renders as:
    
      * italics * or * italics *
    
      * * boldtext * * or moarboldtext
    
      ~~ strickentext ~~
    
      ` some code `
    
      The Unison website
    
      A link to a term:
      Right (Term.Term (Any (_ _eta -> Some _eta))) . A link to
      a type List .
    
      # Escaping formatting
      
        If you want the text.
      
        ``` raw {{ syntax.doc.word "__not bold__"}} ```
      
        Renders as:
      
        __not bold__
      
        If you have some inline text you want to leave unparsed
        and have it render in a monospace font, do:
      
        ``` raw An example of bold text syntax: ''__some bold
        text__'' ```
      
        Renders as:
      
        An example of bold text syntax: ` __some bold text__ `
  
    # Sections and subsections
    
      ``` raw # Section title! Sections consist of section
      titles followed by zero or more paragraphs or other
      section elements (such as subsections). ## Subsection
      title * Item 1 * Item 2 ## An empty subsection! ###
      Subsection 2.1 Some deeply nested content in subsection
      2.1 ```
    
      Sections start with a title, then zero or more documents,
      separated by blanklines. Sections whose title starts with
      ` ## ` are subsections of sections whose title starts with
      one `#`. Sections may be nested arbitrarily.
  
    # Lists
    
      # Bulleted lists
      
        Bulleted lists can use `+`, `-`, or ` * ` for the
        bullets. They can be nested, to any depth:
      
        ``` raw + A + B + C - C1 * C2 ```
      
        Renders as:
      
        * A
        * B
        * C* C1
          * C2
    
      # Numbered lists
      
        ``` raw 1. A 2. B 3. C ```
      
        Renders as:
      
        1. A
        2. B
        3. C
      
        The first number of the list determines the starting
        number in the rendered output. The other numbers are
        ignored:
      
        ``` raw 10. A 99. B 102. C ```
      
        Renders as:
      
        10. A
        11. B
        12. C
      
        Numbered lists can be nested as well, and combined with
        bulleted lists:
      
        ``` raw 1. Wake up. + What am I doing here? + In this
        nested list. 2. Take shower. 3. Get dressed. ```
      
        1. Wake up.* What am I doing here?
           * In this nested list.
        2. Take shower.
        3. Get dressed.

```

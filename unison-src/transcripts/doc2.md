# Test parsing and round-trip of doc2 syntax elements

```ucm:hide
.> builtins.mergeio
```

```unison:hide
otherDoc : a -> Doc2
otherDoc _ = {{ yo }}

otherTerm : Nat
otherTerm = 99

fulldoc : Doc2
fulldoc =
  use Nat +
  {{
Heres some text with a
soft line break

hard line break

Here's a cool **BOLD** __italic__ ~~strikethrough~~ thing with an inline code block ''1 + 2''

# Heading

## Heading 2

Term Link: {otherTerm}

Type Link: {type Optional}

Term source:

@source{term}

Term signature:

@signature{term}

* List item

Inline code:

`` 1 + 2 ``

` "doesn't typecheck" + 1 `

[Link](https://unison-lang.org)

![Image](https://share-next.unison-lang.org/static/unison-logo-circle.png)

Horizontal rule

---

Video

{{
Special
    (Embed
    (Any (Video [MediaSource "test.mp4" None] [("poster", "test.png")])))
}}

Transclusion/evaluation:

{{ otherDoc (a -> Word a) }}

---

The following markdown features aren't supported by the Doc format yet, but maybe will someday


> Block quote


Table

| Header 1 | Header 2 | 
| -------- | -------- | 
| Cell 1 | Cell 2 | 


    Indented Code block

'''
    Exact whitespace should be preserved, don't mess with the logo!
    <- Should be exactly 4 spaces to the left!

  <- Should be 2 spaces here

     _____     _
    |  |  |___|_|___ ___ ___
    |  |  |   | |_ -| . |   |
    |_____|_|_|_|___|___|_|_|
'''

Inline '' text literal with 1 space of padding '' in the middle of a sentence.

}}
```

```ucm
.> debug.format
.> debug.format
```

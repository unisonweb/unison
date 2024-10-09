``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide
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


}}
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> debug.doc-to-markdown fulldoc
```

You can add docs to a term or type with a top-level doc literal above the binding:

``` unison
{{ This is a term doc }}
myTerm = 10

-- Regression tests for https://github.com/unisonweb/unison/issues/4634
{{ This is a type doc }}
type MyType = MyType

{{ This is a unique type doc }}
unique type MyUniqueType = MyUniqueType

{{ This is a structural type doc }}
structural type MyStructuralType = MyStructuralType
```

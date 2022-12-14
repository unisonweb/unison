# Doc rendering

```ucm:hide
.> builtins.mergeio
```

```unison:hide
structural type Maybe a = Nothing | Just a
otherTerm = "text"

otherDoc : (Text -> Doc2) -> Doc2
otherDoc mkMsg = {{
This doc should be embedded.

{{mkMsg "message"}}

}}

{{
# Heading

## Heading 2

Term Link: {otherTerm}

Type Link: {type Maybe}

Term source:

@source{term}

Term signature:

@signature{term}

* List item

1. Numbered list item

> Block quote

    Code block

Inline code:

`` 1 + 2 ``

`
"doesn't typecheck" + 1
`

[Link](https://unison-lang.org)

![Image](https://share-next.unison-lang.org/static/unison-logo-circle.png)

**Bold**

*Italic*

~~Strikethrough~~

Horizontal rule

---

Table

| Header 1 | Header 2 |
| -------- | -------- |
| Cell 1   | Cell 2   |
| Cell 3   | Cell 4   |


Video

{{ Special (Embed (Any (Video [(MediaSource "test.mp4" None)] [("poster", "test.png")]))) }}

Transclusion/evaluation:

{{otherDoc (a -> Word a )}}

}}
term = 42
```

```ucm:hide
.> add
```

```ucm
.> display term.doc
```

```api
GET /api/getDefinition?names=term
```

# Linking definitions to metadata

```ucm:hide
.> builtins.merge
```

The `link` and `unlink` commands can be used to manage metadata linked to definitions. For example, you can link documentation to a definition:

```unison
use .builtin

coolFunction x = x * 2

coolFunction.doc = [: This is a cool function. :]
```

```ucm
.> add
.> link coolFunction.doc coolFunction
```

You can use arbitrary Unison values and link them as metadata to definitions:

```unison
toCopyrightHolder author = match author with
  Author guid name -> CopyrightHolder guid name

alice = Author (GUID Bytes.empty) "Alice Coder"

coolFunction.license = License [toCopyrightHolder alice] [Year 2020] licenses.mit

licenses.mit = LicenseType [:
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
:]
```

```ucm
.> add
.> link coolFunction.license coolFunction
.> link alice coolFunction
```

We can look at the links we have:

```ucm
.> links coolFunction
```

We can link the same metadata simultaneously to multiple definitions:

```unison
myLibrary.f x = x + 1
myLibrary.g x = x + 2
myLibrary.h x = x + 3
```

```ucm
.> add
.> cd myLibrary
.myLibrary> find
.myLibrary> link .alice 1-3
.myLibrary> links f
.myLibrary> links g
.myLibrary> links h
.myLibrary> history
```

Examples of user errors that are handled

```unison:hide
x = 42
x.doc = [: I am the documentation for x :]
a.b.c = 1
```

```ucm:hide:all
.> add
```
1. Trying to link metadata that does not exist to an existing definition
```ucm:error
.> link x.do x
.> link .x.do x
.> link .a.c.d a.b.c
.> link ##x x
```

2. Trying to link non-existent metadata to a non-existent definition
```ucm:error
.> link blah blah
.> link .a.c.d .a.c.d
```

3. Trying to link existing metadata to non-existing definition(s)

```ucm:error
.> link x.doc y
.> link a.b.c a.b.d
.> link .a.b.c .a.b.d
.> link x.doc a .b c .d e .f g
.> link .x.doc a.a.a.a b.b.b.b .c.c.c.c
```

4. Trying to link an existing definition if grouped witn non-existing ones
```ucm:error
.> link x.doc x y
.> link x.doc .x .y
.> links x
.> link x.doc a.b.c a.b.d
.> link x.doc .a.b.c .a.b.d
.> links a.b.c
```

5. Trying to relink an existing link
```ucm
.> link x.doc x
.> link x.doc x
```

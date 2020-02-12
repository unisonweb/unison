# Linking definitions to metadata

The `link` and `unlink` commands can be used to manage metadata linked to definitions. For example, you can link documentation to a definition:


```unison
use .builtin

coolFunction x = x * 2

coolFunction.doc = [: This is a cool function. :]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      coolFunction     : Nat -> Nat
      coolFunction.doc : Doc
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    coolFunction     : Nat -> Nat
    coolFunction.doc : Doc

.> link coolFunction.doc coolFunction

  Updates:
  
    1. coolFunction : Nat -> Nat
       + 2. doc : Doc

```
You can use arbitrary Unison values and link them as metadata to definitions:

```unison
use .builtin 

unique [License] type License = { licenseText : Text }

unique [Author] type Author = { authorName : Text }

unique [Year] type Year = { toInt : Int }

alice = Author "Alice Coder"

coolFunction.license = licenses.mit (Year +2020) alice

licenses.mit : Year -> Author -> License
licenses.mit year author = 
  License ("Copyright " ++ toText (Year.toInt year) ++ " " ++ authorName author ++ "

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the \"Software\"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
")
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Author
      unique type License
      unique type Year
      Author.authorName          : Author -> Text
      Author.authorName.modify   : (Text ->{𝕖} Text)
                                   -> Author
                                   ->{𝕖} Author
      Author.authorName.set      : Text -> Author -> Author
      License.licenseText        : License -> Text
      License.licenseText.modify : (Text ->{𝕖} Text)
                                   -> License
                                   ->{𝕖} License
      License.licenseText.set    : Text -> License -> License
      Year.toInt                 : Year -> Int
      Year.toInt.modify          : (Int ->{𝕖} Int)
                                   -> Year
                                   ->{𝕖} Year
      Year.toInt.set             : Int -> Year -> Year
      alice                      : Author
      coolFunction.license       : License
      licenses.mit               : Year -> Author -> License
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    unique type Author
    unique type License
    unique type Year
    Author.authorName          : Author -> Text
    Author.authorName.modify   : (Text ->{𝕖} Text)
                                 -> Author
                                 ->{𝕖} Author
    Author.authorName.set      : Text -> Author -> Author
    License.licenseText        : License -> Text
    License.licenseText.modify : (Text ->{𝕖} Text)
                                 -> License
                                 ->{𝕖} License
    License.licenseText.set    : Text -> License -> License
    Year.toInt                 : Year -> Int
    Year.toInt.modify          : (Int ->{𝕖} Int)
                                 -> Year
                                 ->{𝕖} Year
    Year.toInt.set             : Int -> Year -> Year
    alice                      : Author
    coolFunction.license       : License
    licenses.mit               : Year -> Author -> License

.> link coolFunction.license coolFunction

  Updates:
  
    1. coolFunction : Nat -> Nat
       + 2. license : License

.> link alice coolFunction

  Updates:
  
    1. coolFunction : Nat -> Nat
       + 2. alice : Author

```
We can look at the links we have:

```ucm
.> links coolFunction

  1. coolFunction.license : License
  2. alice                : Author
  3. coolFunction.doc     : Doc
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

```
We can link the same metadata simultaneously to multiple definitions:

```unison
myLibrary.f x = x + 1
myLibrary.g x = x + 2
myLibrary.h x = x + 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      myLibrary.f : Nat -> Nat
      myLibrary.g : Nat -> Nat
      myLibrary.h : Nat -> Nat
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    myLibrary.f : Nat -> Nat
    myLibrary.g : Nat -> Nat
    myLibrary.h : Nat -> Nat

.> cd myLibrary

.myLibrary> find

  1. f : Nat -> Nat
  2. g : Nat -> Nat
  3. h : Nat -> Nat
  

.myLibrary> link .alice 1-3

  Updates:
  
    1. myLibrary.f : Nat -> Nat
       + 2. alice : Author
    
    3. myLibrary.g : Nat -> Nat
       + 4. alice : Author
    
    5. myLibrary.h : Nat -> Nat
       + 6. alice : Author

.myLibrary> links f

  1. .alice : Author
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.myLibrary> links g

  1. .alice : Author
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.myLibrary> links h

  1. .alice : Author
  
  Tip: Try using `display 1` to display the first result or
       `view 1` to view its source.

.myLibrary> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ #6otbhs4j9a
  
    
  
  ⊙ #83aqs82gho
  
    + Adds / updates:
    
      h
  
  ⊙ #eisjldgdgg
  
    + Adds / updates:
    
      g
  
  ⊙ #seb6uuoocl
  
    + Adds / updates:
    
      f
  
  □ #7asfbtqmoj (start of history)

```

" Vim syntax file, adapted from Haskell file by John Williams <jrw@pobox.com>
"
" Language:		unison
" Maintainer:		Unison Computing
" Last Change:		Aug 23, 2018
" Original Author:      Paul Chiusano and Rúnar Bjarnason
"
" Options-assign a value to these variables to turn the option on:
"
" u_highlight_delimiters - Highlight delimiter characters--users
"			    with a light-colored background will
"			    probably want to turn this on.
" u_highlight_boolean - Treat True and False as keywords.
" u_highlight_types - Treat names of primitive types as keywords.
" u_highlight_more_types - Treat names of other common types as keywords.
" u_highlight_debug - Highlight names of debugging functions.
" u_allow_hash_operator - Don't highlight seemingly incorrect C
"			   preprocessor directives but assume them to be
"			   operators
"
" 2018 Aug 23: Adapt Haskell highlighting to Unison, cleanup.
" 2004 Feb 19: Added C preprocessor directive handling, corrected eol comments
"	       cleaned away literate unison support (should be entirely in
"	       lunison.vim)
" 2004 Feb 20: Cleaned up C preprocessor directive handling, fixed single \
"	       in eol comment character class
" 2004 Feb 23: Made the leading comments somewhat clearer where it comes
"	       to attribution of work.

" Remove any old syntax stuff hanging around
if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

" (Qualified) identifiers (no default highlighting)
syn match ConId "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[A-Z][a-zA-Z0-9_']*\>"
syn match VarId "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[a-z][a-zA-Z0-9_']*\>"

" Infix operators--most punctuation characters and any (qualified) identifier
" enclosed in `backquotes`. An operator starting with : is a constructor,
" others are variables (e.g. functions).
syn match uVarSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[-!#$%&\*\+/<=>\?@\\^|~.][-!#$%&\*\+/<=>\?@\\^|~:.]*"
syn match uConSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=:[-!#$%&\*\+./<=>\?@\\^|~:]*"
syn match uVarSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[a-z][a-zA-Z0-9_']*`"
syn match uConSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[A-Z][a-zA-Z0-9_']*`"

" Reserved symbols--cannot be overloaded.
syn match uDelimiter  "(\|)\|\[\|\]\|,\|_\|{\|}"

" Strings and constants
syn match   uSpecialChar	contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match   uSpecialChar	contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match   uSpecialCharError	contained "\\&\|'''\+"
syn region  uString		start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=uSpecialChar
syn match   uCharacter		"[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'"lc=1 contains=uSpecialChar,uSpecialCharError
syn match   uCharacter		"^'\([^\\]\|\\[^']\+\|\\'\)'" contains=uSpecialChar,uSpecialCharError
syn match   uNumber		"\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
syn match   uFloat		"\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"

" Keyword definitions. These must be patterns instead of keywords
" because otherwise they would match as keywords at the start of a
" "literate" comment (see lu.vim).
syn match uModule		"\<module\>"
syn match uImport		"\<use\>"
syn match uInfix		"\<\(infix\|infixl\|infixr\)\>"
syn match uTypedef		"\<\(∀\|forall\)\>"
syn match uStatement		"\<\(unique\|ability\|type\|where\|match\|cases\|;\|let\|with\|handle\)\>"
syn match uConditional		"\<\(if\|else\|then\)\>"

" Not real keywords, but close.
if exists("u_highlight_boolean")
  " Boolean constants from the standard prelude.
  syn match uBoolean "\<\(true\|false\)\>"
endif
if exists("u_highlight_types")
  " Primitive types from the standard prelude and libraries.
  syn match uType "\<\(Float\|Nat\|Int\|Boolean\|Remote\|Text\)\>"
endif
if exists("u_highlight_more_types")
  " Types from the standard prelude libraries.
  syn match uType "\<\(Optional\|Either\|Sequence\|Effect\)\>"
  syn match uMaybe    "\<None\>"
  syn match uExitCode "\<\(ExitSuccess\)\>"
  syn match uOrdering "\<\(GT\|LT\|EQ\)\>"
endif
if exists("u_highlight_debug")
  " Debugging functions from the standard prelude.
  syn match uDebug "\<\(undefined\|error\|trace\)\>"
endif


" Comments
syn match   uLineComment      "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$"
syn region  uBlockComment     start="{-"  end="-}" contains=uBlockComment
syn region  uPragma	       start="{-#" end="#-}"
syn region  uBelowFold	       start="^---" skip="." end="." contains=uBelowFold

" Docs
syn region  uDocBlock         start="\[:" end=":]" contains=uLink,uDocDirective
syn match   uLink             contained "@\([A-Z][a-zA-Z0-9_']*\.\)\=\<[a-z][a-zA-Z0-9_'.]*\>"
syn match   uDocDirective     contained "@\[\([A-Z][a-zA-Z0-9_']*\.\)\=\<[a-z][a-zA-Z0-9_'.]*\>] \(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[a-z][a-zA-Z0-9_'.]*\>"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_u_syntax_inits")
  if version < 508
    let did_u_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink uImport			  Include
  HiLink uInfix			  PreProc
  HiLink uStatement			  Statement
  HiLink uConditional			  Conditional
  HiLink uSpecialChar			  SpecialChar
  HiLink uTypedef			  Typedef
  HiLink uVarSym			  uOperator
  HiLink uConSym			  uOperator
  HiLink uOperator			  Operator
  HiLink uDelimiter			  Delimiter
  HiLink uSpecialCharError		  Error
  HiLink uString			  String
  HiLink uCharacter			  Character
  HiLink uNumber			  Number
  HiLink uFloat			  Float
  HiLink uConditional			  Conditional
  HiLink uLiterateComment		  uComment
  HiLink uBlockComment		  uComment
  HiLink uLineComment			  uComment
  HiLink uComment			  Comment
  HiLink uBelowFold			  Comment
  HiLink uDocBlock                String
  HiLink uLink                    uType
  HiLink uDocDirective            uImport
  HiLink uPragma			  SpecialComment
  HiLink uBoolean			  Boolean
  HiLink uType			  Type
  HiLink uMaybe			  uEnumConst
  HiLink uOrdering			  uEnumConst
  HiLink uEnumConst			  Constant
  HiLink uDebug			  Debug

  delcommand HiLink
endif


let b:current_syntax = "unison"

" Options for vi: ts=8 sw=2 sts=2 nowrap noexpandtab ft=vim

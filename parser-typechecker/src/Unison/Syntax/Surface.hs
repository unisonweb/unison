-- | The Surface IR: a concrete-syntax-neutral presentation of Unison code.
--
-- This is the pluggability boundary for surface syntax, in BOTH directions. The hard, Unison-specific work —
-- variable freshening\/hygiene, name resolution, and detection of surface sugar (lambdas, let-blocks, @if@, @match@,
-- operator sections, tuples, docs, …) — lives ONCE in the core bridge between 'Surface' and the content-addressed AST:
--
-- @
-- text  --(dialect parse)-->  Surface  --(core elaborate)-->  UnisonFile \/ Term   (read)
-- text  <--(dialect render)-- Surface  <--(core lower)------  Term                 (write)
-- @
--
-- A /dialect/ therefore only has to deal with surface spelling: it provides a parser (@text -> 'SFile'@) and a
-- renderer (@'SFile' -> Pretty SyntaxText@). It never touches 'Reference's, the typechecker, alpha-renaming, or sugar
-- recovery. Conversely the core lowering\/elaboration is written once and is shared by every dialect, so the
-- @cases@\/generated-variable hygiene fix (and every future improvement) is inherited by all of them.
--
-- == Design notes
--
-- * Names on the surface are 'SName' = hash-qualified names. Lowering fills them in from a 'PrettyPrintEnv' (what to
--   display); elaboration resolves them back against 'Unison.Names.Names' (what they mean). Local binders use plain
--   'Name's (single segments) that lowering has already made readable and capture-avoiding.
--
-- * Every node carries an 'Ann'. When lowering from the AST it is the node's source annotation (often 'External');
--   when parsing it is the source span, so type errors after elaboration point back into the user's chosen syntax.
--
-- * Operator applications keep their 'Precedence' so that infix dialects (Haskell\/C) can parenthesize correctly while
--   prefix dialects (Clojure) can ignore it. This is the one place we retain enough information for /every/ dialect to
--   make its own layout decisions without re-deriving precedence.
--
-- * The IR deliberately models /surface/ constructs, not the raw AST: e.g. there is no separate @App@ of a binary
--   builtin — that is an 'SBinOp'; @cases@ is recovered to 'SLam' + 'SMatch' with a hygienic parameter name; tuples
--   are 'STuple', not nested @Pair@ constructors. Lowering is responsible for this recovery; elaboration inverts it.
module Unison.Syntax.Surface
  ( -- * Names and literals
    SName,
    SLit (..),

    -- * Terms
    STerm (..),
    STermF (..),
    SParam (..),
    SBinding (..),
    SCase (..),

    -- * Patterns
    SPattern (..),
    SPatternF (..),
    SSeqOp (..),

    -- * Types
    SType (..),
    STypeF (..),

    -- * Declarations and files
    SModifier (..),
    SConstructor (..),
    SDecl (..),
    SWatch (..),
    SFile (..),
  )
where

import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Syntax.Precedence (Precedence)
import Unison.WatchKind (WatchKind)

-- | A name as it appears on the surface: hash-qualified, possibly suffixified. Lowering produces these from a
-- 'Unison.PrettyPrintEnv.PrettyPrintEnv'; elaboration resolves them via 'Unison.Names.Names'.
type SName = HQ.HashQualified Name

-- | A scalar literal.
data SLit
  = SInt Int64
  | SNat Word64
  | SFloat Double
  | SBool Bool
  | SText Text
  | SChar Char
  deriving stock (Eq, Show)

-- | A surface term: an 'Ann'-annotated 'STermF'.
data STerm = STerm {tAnn :: Ann, tOut :: STermF}
  deriving stock (Show)

data STermF
  = -- | A scalar literal.
    SLit SLit
  | -- | A name occurrence: a local variable, a top-level\/builtin reference, or a (nullary) data\/ability constructor.
    -- Elaboration disambiguates against the local scope; lowering records the display name.
    SName SName
  | -- | Prefix application: @head arg1 arg2 …@ (args non-empty).
    SApp STerm [STerm]
  | -- | A binary application of a symbolic operator, retaining its precedence so infix dialects can parenthesize and
    -- prefix dialects can render it as ordinary application.
    SBinOp SName Precedence STerm STerm
  | -- | A lambda with hygienic parameter names.
    SLam [SParam] STerm
  | -- | A non-recursive (sequential) let block.
    SLet [SBinding] STerm
  | -- | A recursive let block.
    SLetRec [SBinding] STerm
  | SIf STerm STerm STerm
  | SAnd STerm STerm
  | SOr STerm STerm
  | -- | A pattern match. (The @cases@ surface sugar is recovered by lowering into 'SLam' + 'SMatch'; a renderer may
    -- re-sugar it.)
    SMatch STerm [SCase]
  | -- | Ability handling: @handle action with handler@.
    SHandle STerm STerm
  | -- | A delayed computation (thunk).
    SDelay STerm
  | -- | A list literal.
    SList [STerm]
  | -- | A tuple literal (recovered from nested @Pair@ constructors).
    STuple [STerm]
  | -- | A type-ascribed term: @term : type@.
    SAnn STerm SType
  | -- | A typed hole \/ blank.
    SHole
  | -- | A term-link literal: @termLink name@.
    STermLink SName
  | -- | A type-link literal: @typeLink name@.
    STypeLink SName
  | -- | A documentation literal, carried as its @{{ … }}@ source text. Doc markup is dialect-independent, so it
    -- round-trips verbatim: 'Unison.Syntax.Surface.Lower' renders the @Doc2@ value to this text, and
    -- 'Unison.Syntax.Surface.Elaborate' re-parses it with the real Unison doc parser.
    SDocLit Text
  deriving stock (Show)

-- | A lambda parameter (a hygienic local name).
data SParam = SParam {pAnn :: Ann, pName :: Name}
  deriving stock (Show)

-- | A single binding, used in let-blocks and at the top level. The optional type is a user-written signature.
data SBinding = SBinding
  { bAnn :: Ann,
    bName :: Name,
    bType :: Maybe SType,
    bValue :: STerm
  }
  deriving stock (Show)

-- | One branch of a 'SMatch'.
data SCase = SCase
  { casePattern :: SPattern,
    caseGuard :: Maybe STerm,
    caseBody :: STerm
  }
  deriving stock (Show)

-- | A surface pattern.
data SPattern = SPattern {patAnn :: Ann, patOut :: SPatternF}
  deriving stock (Show)

data SPatternF
  = SPWild
  | SPVar Name
  | SPLit SLit
  | -- | A (possibly nullary) data-constructor pattern.
    SPCtor SName [SPattern]
  | -- | An as-pattern: @name\@pat@.
    SPAs Name SPattern
  | -- | A list literal pattern: @[p1, p2, …]@.
    SPList [SPattern]
  | -- | A sequence cons\/snoc\/concat pattern.
    SPSeqOp SPattern SSeqOp SPattern
  | -- | A tuple pattern @(p, q, …)@ (2+ elements). Sugar for the underlying @Tuple@ constructor pattern.
    SPTuple [SPattern]
  | -- | An ability-request pattern: @{ Op args -> k }@ (the final pattern is the continuation).
    SPEffect SName [SPattern] SPattern
  | -- | The pure case of an ability pattern: @{ pat }@.
    SPEffectPure SPattern
  deriving stock (Show)

data SSeqOp = SCons | SSnoc | SConcat
  deriving stock (Eq, Show)

-- | A surface type.
data SType = SType {tyAnn :: Ann, tyOut :: STypeF}
  deriving stock (Show)

data STypeF
  = -- | A bound type variable.
    STyVar Name
  | -- | A type reference, by display name.
    STyRef SName
  | -- | A function type @i ->{abilities} o@; the ability row is 'Nothing' for a pure arrow.
    STyArrow SType (Maybe [SType]) SType
  | -- | A universally quantified type.
    STyForall [Name] SType
  | -- | Type application.
    STyApp SType [SType]
  | -- | An ability row, @{e1, e2}@.
    STyEffects [SType]
  | -- | A tuple type @(a, b, …)@ (2+ elements) or the unit type @()@ (zero elements). Sugar for the underlying
    -- @Tuple@\/@Unit@ encoding.
    STyTuple [SType]
  | -- | A type carrying a (non-arrow) ability requirement, @{e1, e2} t@ — e.g. an ability request type @{Abort} a@.
    STyEffectful [SType] SType
  deriving stock (Show)

-- | A data\/ability type modifier.
data SModifier
  = SStructural
  | -- | A unique type, carrying its GUID (so unique-type identity round-trips).
    SUnique Text
  deriving stock (Eq, Show)

-- | A single constructor of a data\/ability declaration.
data SConstructor = SConstructor
  { cAnn :: Ann,
    cName :: Name,
    cType :: SType
  }
  deriving stock (Show)

-- | A data or ability declaration.
data SDecl = SDecl
  { dAnn :: Ann,
    dModifier :: SModifier,
    -- | 'True' for an @ability@ (effect) declaration, 'False' for a @type@ (data) declaration.
    dIsAbility :: Bool,
    dName :: Name,
    dTypeParams :: [Name],
    dConstructors :: [SConstructor],
    -- | @Just fieldNames@ when this is a record: the declaration has a single constructor whose argument types are the
    -- field types (in order). Renderers use this to print record syntax; 'Unison.Syntax.Surface.Elaborate' uses it to
    -- regenerate the field accessors on re-parse, so records round-trip with their accessors intact.
    dFields :: Maybe [Name]
  }
  deriving stock (Show)

-- | A watch expression (@>@ or @test>@).
data SWatch = SWatch
  { wAnn :: Ann,
    wKind :: WatchKind,
    wBody :: STerm
  }
  deriving stock (Show)

-- | A whole surface file: the unit a dialect parses to and renders from.
data SFile = SFile
  { fNamespace :: Maybe Name,
    fDecls :: [SDecl],
    fBindings :: [SBinding],
    fWatches :: [SWatch]
  }
  deriving stock (Show)

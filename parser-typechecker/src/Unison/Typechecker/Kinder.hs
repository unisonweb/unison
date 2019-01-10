{- Unison's kind system -}
module Unison.Typechecker.Kinder where

-- | The kindchecking environment
data KEnv v loc = KEnv
  { env :: Env v loc -- The kindchecking state
  , builtinLocation :: loc -- The location of builtins
  }

type M v loc = ReaderT (KEnv v loc) (Result (Seq (KindError v loc)))

synthesize
  :: forall v loc . (Var v, Ord loc) => Type v loc -> M v loc (Kind v loc)
synthesize t = case t of
  Type.Var' v -> getContext >>= \ctx -> case lookupAnn ctx v of
    Nothing -> compilerCrash $ UndeclaredTypeVariable v ctx
    Just t  -> pure t
  Type.Ann' (Type.Ref' _) k -> case ABT.freeVars k of
    s | Set.null s -> pure k
    s              -> compilerCrash $ FreeVarsInKindAnnotation s
  Type.Ref' h        -> compilerCrash $ UnannotatedReference h
  Type.Ann'  t' k    -> k <$ check t' k
  Type.Apps' c  args -> do
    ck       <- synthesize c
    ctx      <- getContext
    (vs, ck) <- ungeneralize' ck
    synthesizeApps (apply ctx ck) args
  Type.Forall' subst -> do
    [arg, i, o] <- sequence
      [ ABT.freshen subst freshenVar
      , freshenVar (ABT.variable subst)
      , freshNamed "k"
      ]
    let ik = Kind.existential' l B.Blank i
        ok = Kind.existential' l B.Blank o
    appendContext $ context [existential i, existential o, Ann arg ik]
    body <- pure $ ABT.bindInheritAnnotation subst (Term.var () arg)
    check body ot
    ctx <- getContext
    pure ot
  Type.Arrows' _ -> Kind.Type
  where l = loc t

-- TODO: this is exactly synthesizeApps from Context.hs.
-- Potential for code sharing between types and kinds.
synthesizeApps
  :: (Foldable f, Var v, Ord loc)
  => Kind v loc
  -> f (Type v loc)
  -> M v loc (Kind v loc)
synthesizeApps ft args = foldM go ft $ Foldable.toList args `zip` [1 ..]
 where
  go ft arg = do
    ctx <- getContext
    synthesizeApp (apply ctx ft) arg

synthesizeApp
  :: (Var v, Ord loc) => Kind v loc -> (Type v loc, Int) -> M v loc (Kind v loc)
synthesizeApp ck argp@(arg, argNum) = case ck of
  Kind.Forall' body -> do
    v <- ABT.freshen body freshenKindVar
    appendContext (context [existential v])
    let ck2 = ABT.bindInheritAnnotation body (Kind.existential B.blank v)
    synthesizeApp ck2 argp
  Kind.Arrow' i o -> do
    o <$ check arg i
  Kind.Existential' b a -> do
    [i, o] <- traverse freshenVar [ABT.v' "i", ABT.v' "o"]
    let it     = Kind.existential' (loc ck) B.Blank i
        ot     = Kind.existential' (loc ck) B.Blank o
        soln   = Kind.Monokind $ Kind.arrow (loc ck) it ot
        ctxMid = context [existential o, existential i, Solved b a soln]
    modifyContext' $ replace (existential a)
                             ctxMid
                             synthesizeApp
                             (Kind.getPolykind soln)
                             argp
  _ -> getContext >>= \ctx -> failWith $ KindMismatch ctx


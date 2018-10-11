{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-|
Module      : Eucalypt.Core.Syn
Description : Desugar from surface syntax to core syntax
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.Desugar
where

import Control.Monad.State.Strict
import Data.Char (isUpper)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Eucalypt.Core.Anaphora ()
import Eucalypt.Core.Syn as Syn
import Eucalypt.Core.SourceMap
import Eucalypt.Core.Metadata
import Eucalypt.Core.Target
import Eucalypt.Core.Unit
import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast as Ast
import Eucalypt.Syntax.Input


-- | A desugar pass in a state monad to capture paths of targets and
-- other data during translation
data TranslateState = TranslateState
  { trTargets :: [TargetSpec]
  , trStack :: [CoreBindingName]
  , trImports :: [Input]
  , trSourceMap :: SourceMap
  , trBaseSMID :: SMID
  }

newtype Translate a = Translate { unTranslate :: State TranslateState a }
  deriving (Functor, Applicative, Monad, MonadState TranslateState)

instance MonadSupplySMID Translate where
  recordSpan loc = do
    sm <- gets trSourceMap
    base <- gets trBaseSMID
    let (k, sm') = addSpan sm base loc
    modify $ \s -> s {trSourceMap = sm'}
    return k

-- | Initial state
initTranslateState :: SMID -> TranslateState
initTranslateState = TranslateState [] [] [] mempty

-- | Push a key onto the stack to track where we are, considering
-- statically declared blocks as namespaces
pushKey :: CoreBindingName -> Translate ()
pushKey k = modify $ \s -> s {trStack = k : trStack s}

-- | Pop a key off the stack
popKey :: Translate ()
popKey = modify $ \s -> s {trStack = tail $ trStack s}



-- | Transform a literal into its value
desugarLiteral :: SourceSpan -> PrimitiveLiteral -> Translate CoreExpr
desugarLiteral loc lit =
  mint CorePrim loc $
  case lit of
    VInt i -> CoreInt i
    VFloat f -> CoreFloat f
    VStr s -> CoreString s
    VSym s -> CoreSymbol s



-- | Convert an atomic name (op or normal) to a relative name
relativeName :: AtomicName -> CoreRelativeName
relativeName n =
  case n of
    NormalName name -> name
    OperatorName name -> name



-- | Names that aren't in lookup positions need to become variables or
-- builtins as appropriate
name2Var :: SMID -> String -> CoreExpr
name2Var smid n
  | "__" `isPrefixOf` n && isUpper (n !! 2) = CoreBuiltin smid (drop 2 n)
  | otherwise = CoreVar smid n



-- | Ignore splices for now TODO: splice expressions
declarations :: Block -> [Annotated DeclarationForm]
declarations Located{locatee=(Block elements)} = mapMaybe toDecl elements
  where
    toDecl Located{locatee=(Splice _)} = Nothing
    toDecl Located{locatee=(Declaration d)} = Just d



-- | In contexts where single names should become variables (evaluand
-- rather than individual lookup elements for instance), this converts
-- to vars.
varify :: CoreExpr -> CoreExpr
varify (CoreName smid n) = name2Var smid n
varify e = e



-- | Record the current stack as a path into the namespaces with name
-- of target and any associated documentation
recordTarget :: String -> String -> Translate ()
recordTarget targetName targetDoc = do
  stack <- gets trStack
  let target = TargetSpec targetName targetDoc (reverse stack)
  targets <- gets trTargets
  modify $ \s -> s {trTargets = target : targets, trStack = stack}



-- | Record newly found imports in the unit
recordImports :: [Input] -> Translate ()
recordImports imports =
  modify $ \s@TranslateState {trImports = old} -> s {trImports = old ++ imports}


-- | Process names to vars as appropriate for a context where the
-- exprs will be statically bound
interpretForStaticBoundContext :: [CoreExpr] -> [CoreExpr]
interpretForStaticBoundContext exprs =
  zipWith toVar exprs (anon corenull : exprs)
  where
    toVar :: CoreExpr -> CoreExpr -> CoreExpr
    toVar n@(CoreName _ _) (CoreOperator _ InfixLeft _ (CoreBuiltin _ "*DOT*")) =
      n
    toVar (CoreName smid v) _ = name2Var smid v
    toVar e _ = e


-- | Desugar Ast op soup into core op soup (to be cooked into better
-- tree later, once fixity and precedence of all ops is resolved).
--
-- We can insert call and subtitute lookup operators at this stage but
-- we can't identify catenation until we have the fixit of all ops.
--
-- HACK: This assumes that the dot operator is the highest precedence
-- of all operators. We need this to break the circle - we can't
-- handle all the operator fixities until operators are resoloved but we
-- can't resolve without identifying which names are vars to be
-- resolved and which are just lookup keys.
translateSoup :: [Expression] -> Translate CoreExpr
translateSoup items =
  anon CoreOpSoup . interpretForStaticBoundContext . concat <$>
  traverse trans items
  where
    trans :: Expression -> Translate [CoreExpr]
    trans t@Located {locatee = EApplyTuple _} =
      translate t >>= \expr -> return [Syn.callOp, expr]
    trans Located {locatee = EName (OperatorName ".")} = return [lookupOp]
    trans e = (: []) <$> translate e



-- | Extract and translate the metadata annotation from the declaration
translateAnnotation :: Annotated DeclarationForm -> Translate (Maybe CoreExpr)
translateAnnotation Annotated {annotation = Just a} =
  Just . instantiateLet . normaliseMetadata <$> translate a
translateAnnotation _ = return Nothing



-- | Translate a declaration form into the expression that will be
-- bound
translateDeclarationForm ::
     Maybe CoreExpr
  -> CoreBindingName
  -> DeclarationForm
  -> Translate CoreExpr
translateDeclarationForm a _k Located {locatee = form, location = loc} = do
  k <- recordSpan loc
  case form of
    (PropertyDecl _ expr) -> varifyTranslate expr
    (FunctionDecl _ as expr) -> lam k as <$> varifyTranslate expr
    (OperatorDecl _ l r expr) -> newOp k l r <$> varifyTranslate expr
    (LeftOperatorDecl _ x expr) ->
      newUnOp k UnaryPrefix x <$> varifyTranslate expr
    (RightOperatorDecl _ x expr) ->
      newUnOp k UnaryPostfix x <$> varifyTranslate expr
  where
    newOp k l r expr =
      let (fixity, precedence) = determineFixity a
       in CoreOperator k fixity precedence $ lam k [l, r] expr
    newUnOp k fixity x expr =
      let precedence = determinePrecedence a
       in CoreOperator k fixity precedence $ lam k [x] expr
    varifyTranslate = translate >=> return . varify



-- | Check translated metadata expression for targets
checkTarget :: CoreExpr -> Translate ()
checkTarget annot =
  case determineTarget annot of
    Just (tgt, doc) -> recordTarget tgt doc
    Nothing -> return ()



-- | Check translated metadata expression for imports
checkImports :: CoreExpr -> Translate ()
checkImports annot = forM_ (importsFromMetadata annot) recordImports



-- | Translate an AST block to CoreExpr
translateBlock :: SourceSpan -> Block -> Translate CoreExpr
translateBlock loc blk = do
  dforms <-
    forM (declarations blk) $ \d -> do
      a <- translateAnnotation d
      let k = extractKey d
      pushKey k
      (declMeta, valMeta) <-
        case a of
          Just annot -> do
            checkTarget annot
            checkImports annot
            return $ splitAnnotationMetadata annot
          Nothing -> return (Nothing, Nothing)
      expr <- translateDeclarationForm a k (content d)
      popKey
      return (declMeta, k, valMeta, expr)
  b <- body dforms
  mint2 letexp loc (bindings dforms) b
  where
    extractKey Annotated {content = Located {locatee = decl}} =
      let name =
            case decl of
              (PropertyDecl k _) -> k
              (FunctionDecl k _ _) -> k
              (OperatorDecl k _ _ _) -> k
              (LeftOperatorDecl k _ _) -> k
              (RightOperatorDecl k _ _) -> k
       in atomicName name
    bindings =
      map
        (\(_, n, vm, b) ->
           ( n
           , case vm of
               Just m -> withMeta (sourceMapId m) m b
               Nothing -> b))
    body decls = mint Syn.block loc [ref dm name | (dm, name, _, _) <- decls]
    ref annot name =
      case annot of
        Just a -> anon withMeta a $ anon element name (anon var name)
        Nothing -> anon element name (anon var name)



-- | Translates a string literal pattern expression into core
-- representation - currently a concatenation of vars and literals,
-- possibly as a lambda.
translateStringPattern :: SourceSpan -> [StringChunk] -> Translate CoreExpr
translateStringPattern loc cs = do
  smid <- recordSpan loc
  return $
    bindAnaphora (numberAnaphora $ conc smid) >>= \(Reference v) ->
      anon Syn.var v
  where
    sub :: StringChunk -> CoreExp Target
    sub (Interpolation InterpolationRequest {refTarget = t}) =
      anon Syn.app (anon Syn.bif "STR") [anon Syn.var t]
    sub (LiteralContent s) = anon Syn.str s
    exprs = map sub cs
    conc smid =
      Syn.app smid (anon Syn.bif "JOIN") [CoreList smid exprs, anon Syn.str ""]



-- | Descend through the AST, translating to CoreExpr and recording
-- targets and other metadata in a @TargetState@ record as we go
translate :: Expression -> Translate CoreExpr
translate Located {locatee = expr, location = loc} =
  case expr of
    ELiteral lit -> desugarLiteral loc lit
    EBlock blk -> translateBlock loc blk
    EList components ->
      traverse varifyTranslate components >>= mint CoreList loc
    EName n -> mint CoreName loc $ atomicName n
    EOpSoup _ es -> translateSoup es
    EApplyTuple as -> traverse varifyTranslate as >>= mint CoreArgTuple loc
    EStringPattern chunks -> translateStringPattern loc chunks
  where
    varifyTranslate = translate >=> return . varify



-- | Handle unit-level metadata and then begin recursing down
translateUnit :: Unit -> Translate CoreExpr
translateUnit Located { location = l
                      , locatee = Annotated {annotation = a, content = b}
                      } =
  case a of
    Just annot -> do
      m <- translate annot
      checkImports m
      checkTarget m
      e >>= mint2 CoreMeta l m
    Nothing -> e
  where
    e = translate (Located {location = l, locatee = EBlock b})



-- | Shim for old API
desugar :: Expression -> Syn.CoreExpr
desugar = (`evalState` initTranslateState 1) . unTranslate . translate



-- | Translate AST into core syntax and generate target metadata on
-- the way
translateExpressionToCore :: SMID -> Expression -> TranslationUnit
translateExpressionToCore baseSMID ast =
  TranslationUnit
    { truCore = e
    , truTargets = (reverse . trTargets) s
    , truImports = S.fromList $ trImports s
    , truSourceMap = trSourceMap s
    }
  where
    (e, s) =
      runState (unTranslate $ translate ast) $ initTranslateState baseSMID

-- | Translate AST into core syntax and generate target metadata on
-- the way
translateToCore :: SMID -> Unit -> TranslationUnit
translateToCore baseSMID ast =
  TranslationUnit
    { truCore = e
    , truTargets = (reverse . trTargets) s
    , truImports = S.fromList $ trImports s
    , truSourceMap = trSourceMap s
    }
  where
    (e, s) =
      runState (unTranslate $ translateUnit ast) $ initTranslateState baseSMID



-- | A state wrapping monad for tracking SMID between different
-- translations - we need to ensure unique souce map IDs in all trees
type CoreLoad a = StateT SMID IO a

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-|
Module      : Eucalypt.Core.Desugar
Description : Desugar from surface syntax to core syntax
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.Desugar
  ( translateToCore
  , translateExpressionToCore
  , desugar
  , desugarLambda
  -- * Exposed for tests
  , initTranslateState
  , translateSoup
  , translateBlock
  , translate
  , unTranslate
  , varifyLookupTargets
  , desugarLiteral
  ) where

import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.List (intersperse)
import qualified Data.HashMap.Strict.InsOrd as OM
import qualified Data.Set as S
import Eucalypt.Core.Anaphora ()
import Eucalypt.Core.GenLookup (processGenLookup)
import Eucalypt.Core.Import
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
    -- ^ target specifications accumulated during translation
  , trStack :: [CoreBindingName]
    -- ^ maintain stack of keys to calculate target "paths"
  , trImports :: [Input]
    -- ^ imports discovered
  , trSourceMap :: SourceMap
    -- ^ map of source map IDs to source locations
  , trBaseSMID :: SMID
    -- ^ minimum source map id for the translation unit being created
  , trImportHandler :: ImportHandler
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
initTranslateState ::
     SMID           -- ^ source map ID to start from
  -> ImportHandler  -- ^ for reading imports
  -> TranslateState -- ^ initial state
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



-- | Record the current stack as a path into the namespaces with name
-- of target and any associated documentation
recordTarget :: String -> String -> Maybe String -> Translate ()
recordTarget targetName targetDoc targetFormat = do
  stack <- gets trStack
  let target = TargetSpec targetName targetDoc targetFormat (reverse stack)
  targets <- gets trTargets
  modify $ \s -> s {trTargets = target : targets, trStack = stack}



-- | Record newly found imports in the unit
recordImports :: [Input] -> Translate ()
recordImports imports =
  modify $ \s@TranslateState {trImports = old} -> s {trImports = old ++ imports}



-- | Process names to vars as appropriate for a context where the
-- exprs will be statically bound
varifyLookupTargets :: [CoreExpr] -> [CoreExpr]
varifyLookupTargets exprs =
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
  anon CoreOpSoup .
  processGenLookup . varifyLookupTargets . concat <$>
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



-- | Check translated metadata expression for targets and record them
checkTarget :: CoreExpr -> Translate ()
checkTarget annot =
  case determineTarget annot of
    Just (tgt, doc, format) -> recordTarget tgt doc format
    Nothing -> return ()



-- | Check translated metadata expression for imports and record them
checkImports :: CoreExpr -> Translate ()
checkImports annot = do
  handler <- gets trImportHandler
  maybe (return ()) recordImports (readImports handler annot)



-- | Description of all declaration details for producing the combined
-- core let / block which represents the AST block.
data DeclarationFields = DeclarationFields
  { declMeta :: Maybe CoreExpr
    -- ^ declaration metadata for this declaration
  , declKey :: CoreBindingName
    -- ^ key
  , valueMeta :: Maybe CoreExpr
    -- ^ metadata to be shifted down to value level from the declaration
  , valueExpr :: CoreExpr
    -- ^ the translation of the value
  }



-- | Return the binding to use in the 'CoreLet'
asBinding ::
  DeclarationFields -> (CoreBindingName, CoreExp CoreBindingName)
asBinding DeclarationFields{..} =
  ( declKey
  , case valueMeta of
      Just m -> withMeta (sourceMapId m) m valueExpr
      Nothing -> valueExpr)



-- | Extract declarations from the block (ignoring unimplemented
-- splices)
--
declarations :: Block -> [Annotated DeclarationForm]
declarations Located{locatee=(Block elements)} = mapMaybe toDecl elements
  where
    toDecl Located{locatee=(Splice _)} = Nothing
    toDecl Located{locatee=(Declaration d)} = Just d



-- | For each declaration translate and return a unified description
-- gathering together key, value and metadata
unifiedDeclarations :: Block -> Translate [DeclarationFields]
unifiedDeclarations blk =
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
    return $ DeclarationFields declMeta k valMeta expr
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



-- | Maintain a translation state as we process the declarations,
-- allowing us to identify illegal redeclarations.
data BlockTranslationState = BlockTranslationState
  { declMap :: OM.InsOrdHashMap CoreBindingName DeclarationFields
    -- ^ store decls in insert ordered map to check for dupes
  , declRevBindings :: [(CoreBindingName, CoreExpr)]
    -- ^ to accumulate the let bindings
  , declRevElements :: [CoreExpr]
    -- ^ to accumulate the block elements
  }



-- | Initial blank block translation state
initBlockTranslationState :: BlockTranslationState
initBlockTranslationState = BlockTranslationState OM.empty [] []



-- | Add a new declaration into the accumulating state
addNewDecl :: DeclarationFields -> State BlockTranslationState ()
addNewDecl decl@DeclarationFields {..} =
  modify
    (\BlockTranslationState {..} ->
       BlockTranslationState
         { declMap = OM.insert declKey decl declMap
         , declRevBindings = asBinding decl : declRevBindings
         , declRevElements = ref declMeta declKey : declRevElements
         })
  where
    ref annot name =
      case annot of
        Just a -> anon element name (anon withMeta a $ anon var name)
        Nothing -> anon element name (anon var name)



-- | Add a redeclaration into the accumulating state, representing the
-- error in the core expression as a 'CoreRedeclaration'
addRedecl :: DeclarationFields -> State BlockTranslationState ()
addRedecl DeclarationFields {..} =
  modify
    (\BlockTranslationState {..} ->
       BlockTranslationState
         { declMap = declMap
         , declRevBindings = declRevBindings
         , declRevElements =
             CoreRedeclaration (sourceMapId valueExpr) declKey :
             declRevElements
         })



-- | Process a single declaration, adding a declaration or
-- redeclaration as appropriate
processDeclaration :: DeclarationFields -> State BlockTranslationState ()
processDeclaration decl@DeclarationFields{..} = do
  om <- gets declMap
  case OM.lookup declKey om of
    Just _ -> addRedecl decl
    Nothing -> addNewDecl decl



-- | Read bindings and elements from state and return in correct order
bindingsAndElements ::
     State BlockTranslationState ( [(CoreBindingName, CoreExpr)]
                                 , [CoreExpr])
bindingsAndElements = do
  BlockTranslationState{..} <- get
  return (reverse declRevBindings, reverse declRevElements)



-- | Run the state monad, accumulate declarations and return the
-- bindings and elements.
processDeclarations ::
     [DeclarationFields]
  -> ([(CoreBindingName, CoreExpr)], [CoreExpr])
processDeclarations decls =
  flip evalState initBlockTranslationState $
  traverse_ processDeclaration decls >> bindingsAndElements



-- | Translate an AST block to CoreExpr, detecting and marking any
-- erroneous redeclarations in the block.
translateBlock :: SourceSpan -> Block -> Translate CoreExpr
translateBlock loc blk = do
  decls <- unifiedDeclarations blk
  let (bindings, elements) = processDeclarations decls
  b <- mint Syn.block loc elements
  mint2 letblock loc bindings b




-- | Translates a string literal pattern expression into core
-- representation - currently a concatenation of vars and literals,
-- possibly as a lambda.
translateStringPattern :: SourceSpan -> [StringChunk] -> Translate CoreExpr
translateStringPattern loc cs = do
  smid <- recordSpan loc
  return $ processAnaphora (conc smid) >>= reference2Soup
  where
    sub :: StringChunk -> CoreExp Target
    sub (Interpolation InterpolationRequest { refTarget = t
                                            , refParseOrFormat = spec
                                            }) =
      case spec of
        (Just fmt@('%':_)) ->
          anon Syn.app (anon Syn.bif "FMT") [anon Syn.var t, anon Syn.str fmt]
        (Just fmt) ->
          anon Syn.app (anon Syn.var (Reference [fmt])) [anon Syn.var t]
        Nothing -> anon Syn.app (anon Syn.bif "STR") [anon Syn.var t]
    sub (LiteralContent s) = anon Syn.str s
    exprs = map sub cs
    conc smid =
      Syn.app smid (anon Syn.bif "JOIN") [CoreList smid exprs, anon Syn.str ""]
    processAnaphora = bindAnaphora () . numberAnaphora ()
    reference2Soup (Reference [i]) = anon Syn.var i
    reference2Soup (Reference (i:ids)) =
      anon Syn.soup $
      anon Syn.var i :
      lookupOp : intersperse lookupOp (map (anon Syn.corename) ids)
    reference2Soup _ = error "Untransformed anaphora"



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
    e = translate Located {location = l, locatee = EBlock b}



-- | Shim for old API
--
-- Used by YamlSource
desugar :: Expression -> Syn.CoreExpr
desugar =
  (`evalState` initTranslateState 1 nullImportHandler) . unTranslate . translate



-- | Desugar a lambda embedded in foreign syntax
--
-- Used by YamlSource
desugarLambda :: EmbeddedLambda -> Syn.CoreExpr
desugarLambda (Located _ (EmbeddedLambda params expr)) =
  lam 0 params $ desugar expr



-- | Translate AST into core syntax and generate target metadata on
-- the way
translateExpressionToCore ::
     ImportHandler -> SMID -> Expression -> TranslationUnit
translateExpressionToCore handler baseSMID ast =
  TranslationUnit
    { truCore = e
    , truInput = Nothing
    , truTargets = (reverse . trTargets) s
    , truImports = S.fromList $ trImports s
    , truSourceMap = trSourceMap s
    }
  where
    (e, s) =
      runState (unTranslate $ translate ast) $
      initTranslateState baseSMID handler



-- | Translate AST into core syntax and generate target metadata on
-- the way
translateToCore :: ImportHandler -> Input -> SMID -> Unit -> TranslationUnit
translateToCore handler i baseSMID ast =
  TranslationUnit
    { truCore = e
    , truInput = Just i
    , truTargets = (reverse . trTargets) s
    , truImports = S.fromList $ trImports s
    , truSourceMap = trSourceMap s
    }
  where
    (e, s) =
      runState (unTranslate $ translateUnit ast) $
      initTranslateState baseSMID handler

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
import Eucalypt.Core.Metadata
import Eucalypt.Core.Target
import Eucalypt.Core.Unit
import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast as Ast
import Eucalypt.Syntax.Input

-- | Transform a literal into its value
desugarLiteral :: PrimitiveLiteral -> CoreExpr
desugarLiteral lit = CorePrim $
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



-- | Core metadata must be blocks, we apply conventional rules here to
-- massage other shorthands into blocks.
processAnnotation :: CoreExpr -> CoreExpr
processAnnotation s@(CorePrim (CoreString _)) = Syn.block [element "doc" s]
processAnnotation e@(CorePrim (CoreSymbol s))
  | s == "alias" = Syn.block [element "ref" e]
  | s == "suppress" = Syn.block [element "export" e]
  | s == "main" = Syn.block [element "target" e]
  | s == "trace" = Syn.block [element "trace" (CorePrim (CoreBoolean True))]
  | otherwise = Syn.block []
processAnnotation e = e


-- | Names that aren't in lookup positions need to become variables or
-- builtins as appropriate
name2Var :: String -> CoreExpr
name2Var n
  | "__" `isPrefixOf` n && isUpper (n !! 2) = CoreBuiltin (drop 2 n)
  | otherwise = CoreVar n


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
varify (CoreName n) = name2Var n
varify e = e


-- | A desugar pass in a state monad to capture paths of targets and
-- other data during translation
data TranslateState = TranslateState
  { trTargets :: [TargetSpec]
  , trStack :: [CoreBindingName]
  , trImports :: [Input]
  }

newtype Translate a = Translate { unTranslate :: State TranslateState a }
  deriving (Functor, Applicative, Monad, MonadState TranslateState)

-- | Initial state
initTranslateState :: TranslateState
initTranslateState = TranslateState [] [] []

-- | Push a key onto the stack to track where we are, considering
-- statically declared blocks as namespaces
pushKey :: CoreBindingName -> Translate ()
pushKey k = modify $ \s -> s {trStack = k : trStack s}

-- | Pop a key off the stack
popKey :: Translate ()
popKey = modify $ \s -> s {trStack = tail $ trStack s}

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
  CoreOpSoup . makeVars . concat <$> traverse trans items
  where
    trans :: Expression -> Translate [CoreExpr]
    trans t@Located {locatee = EApplyTuple _} = translate t >>= \expr -> return [Syn.callOp, expr]
    trans Located {locatee = EName (OperatorName ".")} = return [lookupOp]
    trans e = (:[]) <$> translate e
    makeVars :: [CoreExpr] -> [CoreExpr]
    makeVars exprs = zipWith toVar exprs (corenull : exprs)
    toVar :: CoreExpr -> CoreExpr -> CoreExpr
    toVar n@(CoreName _) (CoreOperator InfixLeft _ (CoreBuiltin "*DOT*")) = n
    toVar (CoreName v) _ = name2Var v
    toVar e _ = e


-- | Extract and translate the metadata annotation from the declaration
translateAnnotation :: Annotated DeclarationForm -> Maybe CoreExpr
translateAnnotation Annotated {annotation = Just a} =
  flip evalState initTranslateState $
  unTranslate $ Just . instantiateLet . processAnnotation <$> translate a
translateAnnotation _ = Nothing


-- | Translate a declaration form into the expression that will be
-- bound
translateDeclarationForm ::
     Maybe CoreExpr
  -> CoreBindingName
  -> DeclarationForm
  -> Translate CoreExpr
translateDeclarationForm a _k Located {locatee = form} =
  case form of
    (PropertyDecl _ expr) -> varifyTranslate expr
    (FunctionDecl _ as expr) -> lam as <$> varifyTranslate expr
    (OperatorDecl _ l r expr) -> newOp l r <$> varifyTranslate expr
  where
    newOp l r expr =
      let (fixity, precedence) = determineFixity a
       in CoreOperator fixity precedence $ lam [l, r] expr
    varifyTranslate = translate >=> return . varify


-- | Translate an AST block to CoreExpr
translateBlock :: Block -> Translate CoreExpr
translateBlock blk = do
  dforms <-
    forM (declarations blk) $ \d -> do
      let a = translateAnnotation d
      let k = extractKey d
      pushKey k
      case a of
        Just annot -> checkTarget annot >> checkImports annot
        Nothing -> return ()
      expr <- translateDeclarationForm a k (declaration d)
      popKey
      return (a, k, expr)
  return $ letexp (bindings dforms) (body dforms)
  where
    extractKey Annotated {declaration = Located {locatee = decl}} =
      let name =
            case decl of
              (PropertyDecl k _) -> k
              (FunctionDecl k _ _) -> k
              (OperatorDecl k _ _ _) -> k
       in atomicName name
    bindings = map (\(_, n, b) -> (n, b))
    body decls = Syn.block [ref annot name | (annot, name, _) <- decls]
    ref annot name =
      case annot of
        Just a -> withMeta a $ element name (var name)
        Nothing -> element name (var name)
    checkTarget annot =
      case determineTarget annot of
        Just (tgt, doc) -> recordTarget tgt doc
        Nothing -> return ()
    checkImports annot = forM_ (importsFromMetadata annot) recordImports

-- | Translates a string literal pattern expression into core
-- representation - currently a concatenation of vars and literals,
-- possibly as a lambda.
translateStringPattern :: [StringChunk] -> CoreExpr
translateStringPattern cs =
  let exprs = map sub cs
      conc = Syn.app (Syn.bif "JOIN") [CoreList exprs, Syn.str ""]
   in bindAnaphora (numberAnaphora conc) >>= \(Reference v) -> Syn.var v
  where
    sub :: StringChunk -> CoreExp Target
    sub (Interpolation InterpolationRequest {refTarget = t}) =
      Syn.app (Syn.bif "STR") [Syn.var t]
    sub (LiteralContent s) = Syn.str s


-- | Descend through the AST, translating to CoreExpr and recording
-- targets and other metadata in a @TargetState@ record as we go
translate :: Expression -> Translate CoreExpr
translate Located {locatee = expr} =
  case expr of
    ELiteral lit -> return $ desugarLiteral lit
    EBlock blk -> translateBlock blk
    EList components -> CoreList <$> traverse varifyTranslate components
    EName n -> return $ CoreName $ atomicName n
    EOpSoup _ es -> translateSoup es
    EApplyTuple as -> CoreArgTuple <$> traverse varifyTranslate as
    EStringPattern chunks -> return $ translateStringPattern chunks
  where
    varifyTranslate = translate >=> return . varify


-- | Shim for old API
desugar :: Expression -> Syn.CoreExpr
desugar = (`evalState` initTranslateState) . unTranslate . translate


-- | Translate AST into core syntax and generate target metadata on
-- the way
translateToCore :: Expression -> TranslationUnit
translateToCore ast =
  TranslationUnit
    { truCore = e
    , truTargets = (reverse . trTargets) s
    , truImports = S.fromList $ trImports s
    }
  where
    (e, s) = runState (unTranslate $ translate ast) initTranslateState

{-# LANGUAGE LambdaCase #-}
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

import Data.Char (isUpper)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import Eucalypt.Core.Syn as Syn
import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast as Ast

-- | Transform a literal into its value
desugarLiteral :: PrimitiveLiteral -> Primitive
desugarLiteral lit =
  case lit of
    VInt i -> CoreInt i
    VFloat f -> CoreFloat f
    VStr s -> CoreString s
    VSym s -> CoreSymbol s



-- | Convert an atomic name (op or normal) to a binding name
bindingName :: AtomicName -> CoreBindingName
bindingName n =
  case n of
    NormalName name -> name
    OperatorName name -> name



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

-- | Read from unevaluated metadata (expanding out only an outer let
-- to prepare a block for lookup).
readUnevaluatedMetadata :: String -> CoreExpr -> (CoreExpr -> a) -> Maybe a
readUnevaluatedMetadata key expr@(Syn.CoreLet _ _) readVal =
  readUnevaluatedMetadata key (instantiateLet expr) readVal
readUnevaluatedMetadata key (Syn.CoreBlock (Syn.CoreList items)) readVal =
  readVal <$> lookup key buildSearchList
  where
    buildSearchList = mapMaybe kv items
    kv (Syn.CoreList [Syn.CorePrim (Syn.CoreSymbol k), v]) = Just (k, v)
    kv (Syn.CoreMeta _ i) = kv i
    kv _ = Nothing
readUnevaluatedMetadata _ _ _ = Nothing

-- | Precedence classes
precedenceClasses :: [(String, Precedence)]
precedenceClasses =
  [ ("lookup", 100)
  , ("call", 90)
  , ("exp", 85)
  , ("prod", 80)
  , ("sum", 75)
  , ("shift", 60)
  , ("bitwise", 55)
  , ("cmp", 50)
  , ("append", 45)
  , ("eq", 40)
  , ("map", 38)
  , ("bool-prod", 35)
  , ("bool-sum", 30)
  , ("cat", 20)
  , ("meta", 5)
  ]

-- | Use (unevaluated) metadata and default rules to infer fixity and
-- precedence for an operator
determineFixity :: Maybe CoreExpr -> (Fixity, Precedence)
determineFixity (Just meta) = (fixity, fromMaybe 50 prec)
  where
    fixity =
      case readUnevaluatedMetadata "associates" meta symbolName of
        (Just (Just "right")) -> InfixRight
        _ -> InfixLeft
    prec =
      readUnevaluatedMetadata "precedence" meta $ \case
        (CorePrim (CoreInt n)) -> fromInteger n
        (CorePrim (CoreSymbol cls)) -> (fromMaybe 50 (lookup cls precedenceClasses))
        _ -> 50
determineFixity Nothing = (InfixLeft, 50)


-- | Flatten and desugar a declaration form into annotation, name,
-- expression
desugarDeclarationForm ::
     Annotated DeclarationForm -> (Maybe CoreExpr, CoreBindingName, CoreExpr)
desugarDeclarationForm Annotated { annotation = a
                                 , declaration = Located {locatee = decl}
                                 } =
  let annot = processAnnotation . desugar <$> a
   in case decl of
        PropertyDecl k expr -> (annot, bindingName k, desugarDeclExpr expr)
        FunctionDecl k as expr ->
          (annot, bindingName k, lam as (desugarDeclExpr expr))
        OperatorDecl k l r expr ->
          ( annot
          , bindingName k
          , newOp annot (lam [l, r] (desugarDeclExpr expr)))
  where
    desugarDeclExpr = varify . desugar
    newOp annot expr =
      let (fixity, precedence) = determineFixity annot in
        CoreOperator fixity precedence expr

-- | Ignore splices for now TODO: splice expressions
declarations :: Block -> [Annotated DeclarationForm]
declarations Located{locatee=(Block elements)} = mapMaybe toDecl elements
  where
    toDecl Located{locatee=(Splice _)} = Nothing
    toDecl Located{locatee=(Declaration d)} = Just d

-- | Desugar a block expression.
--
-- TODO: anaphora, splice
desugarBlock :: Block -> CoreExpr
desugarBlock blk = letexp bindings value
  where
    bindings = map (\(_, n, b) -> (n, b)) decls
    decls = map desugarDeclarationForm $ declarations blk
    value = CoreBlock $ CoreList [ref annot name | (annot, name, _) <- decls]
    ref annot name =
      case annot of
        Just a ->
          CoreMeta a (CoreList [CorePrim (CoreSymbol name), CoreVar name])
        Nothing -> CoreList [CorePrim (CoreSymbol name), CoreVar name]




-- | Desugar a general identifier 'a.b.c' into a nested lookup against
-- the item identified by the initial component. The initial component
-- is transformed into a 'CoreVar' unless it has a @__@ prefix and is
-- all caps in which case it is assumed to be a builtin.
desugarIdentifier :: [AtomicName] -> CoreExpr
desugarIdentifier components =
  foldl CoreLookup h $ map relativeName (tail components)
  where
    headName = bindingName (head components)
    h =
      if "__" `isPrefixOf` headName && (length headName > 2 && isUpper (headName !! 2))
        then CoreBuiltin (drop 2 headName)
        else CoreVar headName



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
desugarSoup :: [Expression] -> CoreExpr
desugarSoup = CoreOpSoup . makeVars . insertCalls
  where
    insertCalls = concatMap translate
    translate :: Expression -> [CoreExpr]
    translate t@Located {locatee = EApplyTuple _} = [Syn.callOp, desugar t]
    translate Located {locatee = EName (OperatorName ".")} = [lookupOp]
    translate e = [desugar e]
    makeVars :: [CoreExpr] -> [CoreExpr]
    makeVars exprs = zipWith f exprs (corenull : exprs)
    f :: CoreExpr -> CoreExpr -> CoreExpr
    f n@(CoreName _) (CoreOperator InfixLeft _ (CoreBuiltin "*DOT*")) = n
    f (CoreName v) _ = name2Var v
    f e _ = e

-- | In contexts where single names should become variables (evaluand
-- rather than individual lookup elements for instance), this converts
-- to vars.
varify :: CoreExpr -> CoreExpr
varify (CoreName n) = name2Var n
varify e = e

-- | Desugar an expression into core syntax
desugar :: Expression -> CoreExpr
desugar Located {locatee = expr} =
  case expr of
    ELiteral lit -> CorePrim $ desugarLiteral lit
    EBlock blk -> desugarBlock blk
    EList components -> CoreList $ map (varify .desugar) components
    EName n -> CoreName $ bindingName n
    EOpSoup _ es -> desugarSoup es
    EApplyTuple as -> CoreArgTuple (map (varify . desugar) as)

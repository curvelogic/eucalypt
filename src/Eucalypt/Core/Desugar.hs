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
import Eucalypt.Reporting.Location
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Eucalypt.Core.Syn as Syn
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



-- | Flatten and desugar a declaration form into annotation, name,
-- expression
desugarDeclarationForm ::
     Annotated DeclarationForm -> (Maybe CoreExpr, CoreBindingName, CoreExpr)
desugarDeclarationForm Annotated { annotation = a
                                 , declaration = Located {locatee = decl}
                                 } =
  let annot = processAnnotation . desugar <$> a
   in case decl of
        PropertyDecl k expr -> (annot, bindingName k, desugar expr)
        FunctionDecl k as expr ->
          (annot, bindingName k, lam as (desugar expr))
        OperatorDecl k l r expr ->
          (annot, bindingName k, lam [l, r] (desugar expr))



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


-- | Function calls using arg tuple are treated as operator during the
-- fixity / precedence resolution phases but formed into core syntax
-- after that.
callOp :: CoreExpr
callOp = infixl_ 90 (CoreBuiltin "*CALL*")



-- | Name lookup is treated as operator during the fixity / precedence
-- resolution phases but formed into core syntax after that.
lookupOp :: CoreExpr
lookupOp = infixl_ 95 (CoreBuiltin "*DOT*")



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
    translate t@Located{locatee=EApplyTuple _} = [callOp, desugar t]
    translate Located{locatee=EName (OperatorName ".")} = [lookupOp]
    translate e = [desugar e]
    makeVars :: [CoreExpr] -> [CoreExpr]
    makeVars exprs = zipWith f exprs (corenull:exprs)
    f :: CoreExpr -> CoreExpr -> CoreExpr
    f n@(CoreName _) (CoreOperator InfixLeft _ (CoreBuiltin "*DOT*")) = n
    f (CoreName v) _ = CoreVar v
    f e _ = e


-- | Desugar an expression into core syntax
desugar :: Expression -> CoreExpr
desugar Located{locatee=expr} =
  case expr of
    EOperation opName l r ->
      CoreApp (CoreApp (CoreVar (bindingName opName)) (desugar l)) (desugar r)
    EInvocation f as -> app (desugar f) (map desugar as)
    ECatenation obj verb -> CoreApp (desugar verb) (desugar obj)
    EIdentifier components -> desugarIdentifier components
    ELiteral lit -> CorePrim $ desugarLiteral lit
    EBlock blk -> desugarBlock blk
    EList components -> CoreList $ map desugar components
    EName n -> CoreName $ bindingName n
    EOpSoup _ es -> desugarSoup es
    EApplyTuple as -> CoreArgTuple (map desugar as)

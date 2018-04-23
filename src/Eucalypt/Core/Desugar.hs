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

import Eucalypt.Core.Syn
import Eucalypt.Syntax.Ast
import Data.Maybe ( mapMaybe )

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



-- | Desugar a declaration form
desugarDeclarationForm :: DeclarationForm -> (CoreBindingName, CoreExpr)
desugarDeclarationForm decl =
  case decl of
    PropertyDecl k expr -> (bindingName k, desugar expr)
    FunctionDecl k args expr -> (bindingName k, lamexpr args (desugar expr))
    OperatorDecl k l r expr -> (bindingName k, lamexpr [l, r] (desugar expr))



-- | Ignore splices for now TODO: splice expressions
declarations :: Block -> [DeclarationForm]
declarations (Block elements) = mapMaybe toDecl elements
  where
    toDecl (Splice _) = Nothing
    toDecl (Declaration d) = Just $ declaration d



-- | Desugar a block expression.
--
-- TODO: anaphora, splice
desugarBlock :: Block -> CoreExpr
desugarBlock blk = letexp bindings value
  where
    bindings = map desugarDeclarationForm $ declarations blk
    value =
      CoreBlock $
      CoreList
        [ CoreList [CorePrim (CoreSymbol name), CoreVar name]
        | (name, _) <- bindings
        ]



-- | Desugar an expression into core syntax
desugar :: Expression -> CoreExpr
desugar expr =
  case expr of
    EOperation opName l r ->
      CoreApp
        (CoreApp (CoreVar (bindingName opName)) (desugar l))
        (desugar r)
    EInvocation f args -> appexp (desugar f) (map desugar args)
    ECatenation obj verb -> CoreApp (desugar verb) (desugar obj)
    EIdentifier components ->
      foldl CoreLookup (CoreVar (bindingName (head components))) $
      map relativeName (tail components)
    ELiteral lit -> CorePrim $ desugarLiteral lit
    EBlock blk -> desugarBlock blk
    EList components -> CoreList $ map desugar components

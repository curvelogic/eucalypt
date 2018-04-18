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
    VInt i -> Int i
    VFloat f -> Float f
    VStr s -> String s
    VSym s -> Symbol s

-- | Convert an atomic name (op or normal) to a binding name
bindingName :: AtomicName -> CoreBindingName
bindingName n = case n of
  NormalName name -> name
  OperatorName name -> name

-- | Convert an atomic name (op or normal) to a relative name
relativeName :: AtomicName -> CoreRelativeName
relativeName n = case n of
  NormalName name -> name
  OperatorName name -> name

desugarDeclarationFormExp :: DeclarationForm -> (CoreBindingName, CoreExpr)
desugarDeclarationFormExp decl =
  case decl of
    PropertyDecl k expr -> (bindingName k, desugarExp expr)
    FunctionDecl k args expr -> (bindingName k, lamexpr args (desugarExp expr))
    OperatorDecl k l r expr -> (bindingName k, lamexpr [l, r] (desugarExp expr))

-- | Ignore splices for now TODO: splice expressions
declarations :: Block -> [DeclarationForm]
declarations (Block elements) = mapMaybe toDecl elements
  where toDecl (Splice _) = Nothing
        toDecl (Declaration d) = Just $ declaration d

desugarBlockExp :: Block -> CoreExpr
desugarBlockExp blk = letexp bindings value
  where bindings = map desugarDeclarationFormExp $ declarations blk
        value = CoreBlock $ CoreList [CoreList [CorePrim (Symbol name), CoreVar name] | (name, _) <- bindings]

desugarExp :: Expression -> CoreExpr
desugarExp expr = case expr of
    EOperation opName l r -> CoreApp (CoreApp (CoreVar (bindingName opName)) (desugarExp l)) (desugarExp r)
    EInvocation f args -> appexp (desugarExp f) (map desugarExp args)
    ECatenation obj verb -> CoreApp (desugarExp verb) (desugarExp obj)
    EIdentifier components -> foldl CoreLookup (CoreVar (bindingName (head components))) $ map relativeName (tail components)
    ELiteral lit -> CorePrim $ desugarLiteral lit
    EBlock blk -> desugarBlockExp blk
    EList components -> CoreList $ map desugarExp components

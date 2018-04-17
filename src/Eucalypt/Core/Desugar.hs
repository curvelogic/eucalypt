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
    VInt lit -> Int lit
    VFloat lit -> Float lit
    VStr lit -> String lit
    VSym lit -> Symbol lit

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
    PropertyDecl id expr -> (bindingName id, desugarExp expr)
    FunctionDecl id args expr -> (bindingName id, lamexpr args (desugarExp expr))
    OperatorDecl id l r expr -> (bindingName id, lamexpr [l, r] (desugarExp expr))

-- | Ignore splices for now TODO: splice expressions
declarations :: Block -> [DeclarationForm]
declarations (Block elements) = mapMaybe toDecl elements
  where toDecl (Splice _) = Nothing
        toDecl (Declaration d) = Just $ declaration d

desugarBlockExp :: Block -> CoreExpr
desugarBlockExp block = letexp bindings value
  where bindings = map desugarDeclarationFormExp $ declarations block
        value = CoreBlock $ CoreList [CoreList [CorePrim (Symbol name), CoreVar name] | (name, _) <- bindings]

desugarExp :: Expression -> CoreExpr
desugarExp expr = case expr of
    EOperation id l r -> CoreApp (CoreApp (CoreVar (bindingName id)) (desugarExp l)) (desugarExp r)
    EInvocation f args -> appexp (desugarExp f) (map desugarExp args)
    ECatenation obj verb -> CoreApp (desugarExp verb) (desugarExp obj)
    EIdentifier components -> foldl CoreLookup (CoreVar (bindingName (head components))) $ map relativeName (tail components)
    ELiteral lit -> CorePrim $ desugarLiteral lit
    EBlock block -> desugarBlockExp block
    EList components -> CoreList $ map desugarExp components

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

-- | Transform a SimpleIdentifier into a Name
toName :: AtomicName -> Name
toName (NormalName id) = fromStr id
toName (OperatorName id) = fromStr id

-- | Transform a literal into its value
desugarLiteral :: PrimitiveLiteral -> Primitive
desugarLiteral lit =
  case lit of
    VInt lit -> Int lit
    VFloat lit -> Float lit
    VStr lit -> String lit
    VSym lit -> Symbol lit

-- | Transform a declaration into the name and expression for binding
desugarDeclarationForm :: DeclarationForm -> (Name, InitialCoreExpr)
desugarDeclarationForm decl =
  case decl of
    PropertyDecl id expr -> (toName id, desugar expr)
    FunctionDecl id args expr -> (toName id, lam (map toName args) (desugar expr))
    OperatorDecl id l r expr -> (toName id, lam [toName l, toName r] (desugar expr))

-- | Ignore splices for now
declarations :: Block -> [DeclarationForm]
declarations (Block elements) = mapMaybe toDecl elements
  where toDecl (Splice _) = Nothing
        toDecl (Declaration d) = Just $ declaration d

-- | Transform a block expression into a let which binds the names and
-- returns the block value. TODO: splices
desugarBlock :: Block -> InitialCoreExpr
desugarBlock block = letrec bindings value
  where bindings = map desugarDeclarationForm $ declarations block
        value = BlockValue [(Prim (Symbol $ localName name), Var name) | (name, expr) <- bindings ]

-- | Transform an AST Expression into Core `a` is the binder type
desugar :: Expression -> InitialCoreExpr
desugar expr =
  case expr of
    EOperation id l r -> App (App (Var (toName id)) (desugar l)) (desugar r)
    EInvocation f args -> foldl App (desugar f) (map desugar args)
    ECatenation obj verb -> App (desugar verb) (desugar obj)
    EIdentifier components -> foldl Lookup (Var $ toName (head components)) $ map toName $ tail components
    ELiteral lit -> Prim $ desugarLiteral lit
    EBlock block -> desugarBlock block
    EList components -> ListValue $ map desugar components

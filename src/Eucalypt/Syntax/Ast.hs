{-|
Module      : Eucalypt.Syntax.Ast
Description : Abstract syntax for the Eucalypt language
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Syntax.Ast
  ( ident
  , op
  , cat
  , invoke
  , int
  , prop
  , func
  , oper
  , list
  , str
  , sym
  , ann
  , bare
  , block
  , PrimitiveLiteral(..)
  , AtomicName(..)
  , BlockElement(..)
  , Annotated(..)
  , DeclarationForm(..)
  , Expression(..)
  , Block(..)
  )
where

import Text.Regex

-- | Eucalypt literals: numbers, strings or symbols.
data PrimitiveLiteral = VInt Integer   -- ^ integer (decimal representation)
                      | VFloat Double  -- ^ floating point (TODO: inf, nan?)
                      | VStr String    -- ^ double quoted string
                      | VSym String    -- ^ symbol (colon prefixed)
  deriving (Eq, Show)


-- | Basic unqualified identifier components, name or operator.
data AtomicName
  = NormalName String    -- ^ a name component, simple or single quoted
  | OperatorName String  -- ^ an operator name
  deriving (Eq, Show)


-- | An Expression is anything that can appear to the right of a colon
-- in a declaration.
data Expression

  = EIdentifier [AtomicName]
    -- ^ a (possibly complex) identifier

  | EOperation AtomicName Expression Expression
    -- ^ a binary operator

  | ELiteral PrimitiveLiteral
    -- ^ a primitive literal value

  | EInvocation Expression [Expression]
    -- ^ a function-style invocation @f(a,b,c)@

  | ECatenation Expression Expression
    -- ^ invocation by catenation: @x f@

  | EMeta Expression Expression
    -- ^ application of expression metadata TODO: needed?

  | EBlock Block
    -- ^ a block literal { decls }

  | EList [Expression]
    -- ^ a list literal: [x, y, z]

  deriving (Eq, Show)


-- | A syntax element could be annotated with another expression
data Annotated a
  = Annotated { annotation :: Maybe Expression,
                -- ^ arbitrary expression used to annotated the declaration
                declaration :: a
                -- ^ the declaration itself
              }
  deriving (Eq, Show)


-- | Declaration forms permissible within a block.
--
-- This may be any of
-- * a property declaration
-- * a function declaration
-- * an operator declaration
data DeclarationForm

  = PropertyDecl AtomicName Expression |
    -- ^ A simple property declaration: @key: value-expression@

    FunctionDecl AtomicName [AtomicName] Expression |
    -- ^ A function declaration @f(x, y, z): value-expression@

    OperatorDecl AtomicName AtomicName AtomicName Expression
    -- ^ A binary operator declaration @(x ** y): value-expression@

  deriving (Eq, Show)

-- | Block elements may be annotated declarations or splice forms
-- which will evaluate to alists mapping symbols to values and be
-- merged into the block at evaluation time.
data BlockElement = Declaration (Annotated DeclarationForm)
                  | Splice Expression
  deriving (Eq, Show)

-- | A block is a sequence of block elements, with later keys
-- overriding earlier.
newtype Block = Block [BlockElement]
  deriving (Eq, Show)

-- | Strip single quotes from single-quoted string
unquote :: String -> String
unquote ('\'' : xs) = (reverse . unquote . reverse) xs
unquote xs = xs

-- | Turn dotted string into complex identifier
ident :: String -> Expression
ident = EIdentifier . map (NormalName . unquote) . splitRegex (mkRegex "\\.")

-- | Form an operation from name and operands
op :: String -> Expression -> Expression -> Expression
op = EOperation . OperatorName

-- | Form a catenation of two expressions
cat :: Expression -> Expression -> Expression
cat = ECatenation

-- | Form a multi-argument invocation
invoke :: Expression -> [Expression] -> Expression
invoke = EInvocation

-- | Form a literal int
int :: Integer -> Expression
int = ELiteral . VInt

-- | Form a literal string
str :: String -> Expression
str = ELiteral . VStr

-- | Form a literal symbol
sym :: String -> Expression
sym = ELiteral . VSym

-- | Form a list literal from a list of expressions
list :: [Expression] -> Expression
list = EList

-- | Create a property declaration
prop :: String -> Expression -> DeclarationForm
prop k = PropertyDecl (NormalName k)

-- | Create a function declaration
func :: String -> [String] -> Expression -> DeclarationForm
func f as = FunctionDecl (NormalName f) (map NormalName as)

-- | Create an operator declaration
oper :: String -> String -> String -> Expression -> DeclarationForm
oper o l r = OperatorDecl (OperatorName o)
                          (NormalName l)
                          (NormalName r)

-- | Create an annotated block element
ann :: Expression -> DeclarationForm -> BlockElement
ann a decl = Declaration Annotated { annotation = Just a, declaration = decl }

-- | Create an unannotated block element
bare :: DeclarationForm -> BlockElement
bare decl = Declaration Annotated { annotation = Nothing, declaration = decl }

-- | Create a block expression
block :: [BlockElement] -> Expression
block = EBlock . Block

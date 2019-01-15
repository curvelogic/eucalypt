{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-|
Module      : Eucalypt.Syntax.Ast
Description : Abstract syntax for the Eucalypt language
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Syntax.Ast
where

import GHC.Generics
import Data.Aeson
import Eucalypt.Core.Anaphora
import Eucalypt.Reporting.Location


-- | Eucalypt literals: numbers, strings or symbols.
data PrimitiveLiteral = VInt Integer   -- ^ integer (decimal representation)
                      | VFloat Double  -- ^ floating point (TODO: inf, nan?)
                      | VStr String    -- ^ double quoted string
                      | VSym String    -- ^ symbol (colon prefixed)
  deriving (Eq, Show, Generic, ToJSON)


-- | Basic unqualified identifier components, name or operator.
data AtomicName
  = NormalName String    -- ^ a name component, simple or single quoted
  | OperatorName String  -- ^ an operator name
  deriving (Eq, Show, Generic, ToJSON)



-- | Extract name as string
atomicName :: AtomicName -> String
atomicName (NormalName s) = s
atomicName (OperatorName s) = s



-- | A parameter name is lexically a normal name but just an alias to
-- a string
type ParameterName = String


-- | In a while we may allow configurable bracket types ("idiot
-- brackets") which will affect the interpretation of catenation
-- within them, so we record the bracket set with the op soup.
data BracketSet = Explicit Char Char | Implicit
  deriving (Eq, Show, Generic, ToJSON)


-- | For now though it's just parentheses.
parentheses :: BracketSet
parentheses = Explicit '(' ')'



implicit :: BracketSet
implicit = Implicit

-- | The target of an interpolation request, either an implicit
-- parameter ("anaphor"), numbered or otherwise or a name referring to
-- value in appropriate block.
data Target
  = Anaphor (Maybe Int) -- empty anaphor or indexed anaphor
  | Reference String -- reference to value from block (or environment)
  deriving (Eq, Show, Generic, ToJSON)

instance Anaphora () Target where
  unnumberedAnaphor _ = Anaphor Nothing
  isAnaphor _ (Anaphor _) = True
  isAnaphor _ _ = False
  toNumber _ (Anaphor n) = n
  toNumber _ _ = Nothing
  fromNumber _ = Anaphor . Just
  toName _ (Anaphor (Just n)) = '_':show n
  toName _ _ = ""

-- | The interpolation request
--
-- A target and string governing how to format it
data InterpolationRequest = InterpolationRequest
  { refTarget :: Target
  , refParseOrFormat :: Maybe String
  , refConversion :: Maybe String
  } deriving (Eq, Show, Generic, ToJSON)

-- | A string element is either literal content or an interpolation
-- request
data StringChunk
  = LiteralContent String
  | Interpolation InterpolationRequest
  deriving (Eq, Show, Generic, ToJSON)


type Expression = Located Expression_

-- | An Expression is anything that can appear to the right of a colon
-- in a declaration.
data Expression_

  = ELiteral PrimitiveLiteral
    -- ^ a primitive literal value

  | EBlock Block
    -- ^ a block literal { decls }

  | EList [Expression]
    -- ^ a list literal: [x, y, z]

  | EOpSoup BracketSet [Expression]
    -- ^ a sequence of expressions and operators

  | EName AtomicName
    -- ^ simple identifier

  | EStringPattern [StringChunk]
    -- ^ a string with interpolation / parse elements - e.g. "foo{x}bar"

  | EApplyTuple [Expression]
    -- ^ a tuple in apply-to-params context, e.g. @_(a,b,c)@

  deriving (Eq, Show, Generic, ToJSON)

-- | Strip location from a located expression; useful for testing.
-- TODO: generalise
instance HasLocation Expression_ where
  stripLocation (EBlock b) = EBlock (stripLocation b)
  stripLocation (EList es) = EList (map stripLocation es)
  stripLocation (EOpSoup bs es) = EOpSoup bs (map stripLocation es)
  stripLocation (EApplyTuple es) = EApplyTuple (map stripLocation es)
  stripLocation e = e


-- | A syntax element could be annotated with another expression
data Annotated a
  = Annotated { annotation :: Maybe Expression,
                -- ^ arbitrary expression used to annotated the declaration
                content :: a
                -- ^ the declaration / syntax element itself
              }
  deriving (Eq, Show, Generic, ToJSON)

instance HasLocation a => HasLocation (Annotated a) where
  stripLocation Annotated {annotation = a, content = c} =
    Annotated {annotation = stripLocation <$> a, content = stripLocation c}

-- | Units are block but have optional top-level metadata annotations
type Unit = Located (Annotated Block)

-- | A declaration form has source location metadata
type DeclarationForm = Located DeclarationForm_

-- | Declaration forms permissible within a block.
--
-- This may be any of
-- * a property declaration
-- * a function declaration
-- * an operator declaration (binary, left or right unary)
data DeclarationForm_

  = PropertyDecl AtomicName Expression
    -- ^ A simple property declaration: @key: value-expression@

  | FunctionDecl AtomicName [ParameterName] Expression
    -- ^ A function declaration @f(x, y, z): value-expression@

  | OperatorDecl AtomicName ParameterName ParameterName Expression
    -- ^ A binary operator declaration @(x ** y): value-expression@

  | LeftOperatorDecl AtomicName ParameterName Expression
    -- ^ A left unary operator declaration @(! x): value-expression@

  | RightOperatorDecl AtomicName ParameterName Expression
    -- ^ A right unary operator declaration @(x !): value-expression@

  deriving (Eq, Show, Generic, ToJSON)


instance HasLocation DeclarationForm_ where
  stripLocation (PropertyDecl n e) = PropertyDecl n (stripLocation e)
  stripLocation (FunctionDecl f ps e) = FunctionDecl f ps (stripLocation e)
  stripLocation (OperatorDecl n l r e) = OperatorDecl n l r (stripLocation e)
  stripLocation (LeftOperatorDecl n o e) = LeftOperatorDecl n o (stripLocation e)
  stripLocation (RightOperatorDecl n o e) = RightOperatorDecl n o (stripLocation e)

type BlockElement = Located BlockElement_

-- | Block elements may be annotated declarations or splice forms
-- which will evaluate to alists mapping symbols to values and be
-- merged into the block at evaluation time.
data BlockElement_ = Declaration (Annotated DeclarationForm)
                   | Splice Expression
  deriving (Eq, Show, Generic, ToJSON)

instance HasLocation BlockElement_ where
  stripLocation (Declaration Annotated {annotation = a, content = d}) =
    Declaration Annotated{annotation = stripLocation <$> a, content = stripLocation d}
  stripLocation (Splice e) = Splice $ stripLocation e

-- | A Block is a block with location metadata
type Block = Located Block_

-- | A block is a sequence of block elements, with later keys
-- overriding earlier.
newtype Block_ = Block [BlockElement]
  deriving (Eq, Show, Generic, ToJSON)


instance HasLocation Block_ where
  stripLocation (Block elts) = Block (map stripLocation elts)

-- | Strip single quotes from single-quoted string
unquote :: String -> String
unquote ('\'' : xs) = (reverse . unquote . reverse) xs
unquote xs = xs

-- | Form a literal int
int :: Integer -> Expression
int = at nowhere . ELiteral . VInt

-- | Form a literal string
str :: String -> Expression
str = at nowhere . ELiteral . VStr

-- | Form a literal symbol
sym :: String -> Expression
sym = at nowhere . ELiteral . VSym

-- | Form a list literal from a list of expressions
list :: [Expression] -> Expression
list = at nowhere . EList

-- | Create a property declaration
prop :: String -> Expression -> DeclarationForm
prop k = at nowhere . PropertyDecl (NormalName k)

-- | Create a function declaration
func :: String -> [String] -> Expression -> DeclarationForm
func f params e = at nowhere $ FunctionDecl (NormalName f) params e

-- | Create an operator declaration
oper :: String -> String -> String -> Expression -> DeclarationForm
oper o l r e = at nowhere $ OperatorDecl (OperatorName o) l r e

-- | Create an operator declaration
loper :: String -> String -> Expression -> DeclarationForm
loper o x e = at nowhere $ LeftOperatorDecl (OperatorName o) x e

-- | Create an operator declaration
roper :: String -> String -> Expression -> DeclarationForm
roper o x e = at nowhere $ RightOperatorDecl (OperatorName o) x e

-- | Create an annotated block element
ann :: Expression -> DeclarationForm -> BlockElement
ann a decl = at nowhere $ Declaration Annotated { annotation = Just a, content = decl }

-- | Create an unannotated block element
bare :: DeclarationForm -> BlockElement
bare decl = at nowhere $ Declaration Annotated { annotation = Nothing, content = decl }

-- | Create a block expression
block :: [BlockElement] -> Expression
block = at nowhere . EBlock . at nowhere . Block

-- | Create an annotated unit
bareUnit :: [BlockElement] -> Unit
bareUnit els =
  at nowhere Annotated {annotation = Nothing, content = at nowhere $ Block els}

-- | Create an annotated unit
annUnit :: Expression -> [BlockElement] -> Unit
annUnit a els =
  at nowhere Annotated {annotation = Just a, content = at nowhere $ Block els}

-- | Create an op soup expression with implicit bracketset
opsoup :: [Expression] -> Expression
opsoup = at nowhere . EOpSoup implicit

-- | Create an op soup expression with bracket set parentheses
opsoupParens :: [Expression] -> Expression
opsoupParens = at nowhere . EOpSoup parentheses

-- | A normal name as expression
normalName :: String -> Expression
normalName = at nowhere . EName . NormalName

-- | An operator name as expression
operatorName :: String -> Expression
operatorName = at nowhere . EName . OperatorName

-- | A tuple of args for a fn call (appears in op soup and binds left)
applyTuple :: [Expression] -> Expression
applyTuple = at nowhere . EApplyTuple

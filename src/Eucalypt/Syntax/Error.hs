{-|
Module      : Eucalypt.Syntax.Error
Description : Syntax errors from source or input specs
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Syntax.Error where

import Control.Exception.Safe
import Data.List.NonEmpty as NE
import Data.Void
import Eucalypt.Reporting.Common
import Eucalypt.Reporting.Classes
import Eucalypt.Reporting.Location
import qualified Text.Megaparsec as M


newtype SyntaxError
  = MegaparsecError (M.ParseError (M.Token String) Void)
  deriving (Show, Eq, Typeable)

instance Exception SyntaxError

toSpan :: NonEmpty M.SourcePos -> SourceSpan
toSpan positions = (h, h)
  where
    h = SourcePosition $ NE.head positions

-- | Make SyntaxError 'Reportable'
instance Reportable SyntaxError where
  code (MegaparsecError pe) = Just . toSpan . M.errorPos $ pe
  report (MegaparsecError pe) = standardReport "SYNTAX ERROR" msg
    where msg = M.parseErrorPretty pe

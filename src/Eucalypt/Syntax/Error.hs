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
  = MegaparsecError (M.ParseErrorBundle String Void)
  deriving (Show, Eq, Typeable)

instance Exception SyntaxError

toSpan :: M.SourcePos -> SourceSpan
toSpan p = (h, h)
  where
    h = SourcePosition p

-- | Make SyntaxError 'Reportable'
instance Reportable SyntaxError where
  code (MegaparsecError peb) = Just . toSpan . M.pstateSourcePos $ pstate
    where
      pe1 = NE.head $ M.bundleErrors peb
      (_, pstate) = M.reachOffset (M.errorOffset pe1) (M.bundlePosState peb)
  report (MegaparsecError peb) = standardReport "SYNTAX ERROR" msg
    where
      msg = M.parseErrorTextPretty pe1
      pe1 = NE.head $ M.bundleErrors peb

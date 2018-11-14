{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Eucalypt.Syntax.Error
Description : Aggregated error type for all types of errors
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Reporting.Error
  where

import Control.Applicative ((<|>))
import Eucalypt.Core.SourceMap
import Control.Exception.Safe
import qualified Eucalypt.Core.Error as Core
import qualified Eucalypt.Driver.Error as Driver
import Eucalypt.Reporting.Classes
import qualified Eucalypt.Source.Error as Source
import qualified Eucalypt.Stg.CallStack as CS
import qualified Eucalypt.Stg.Error as Stg
import qualified Eucalypt.Syntax.Error as Syntax
import qualified Text.PrettyPrint as P

-- | All the types of error that Eucalypt can experience and report
data EucalyptError
  = Core Core.CoreError
  | Source Source.DataParseException
  | Syntax Syntax.SyntaxError
  | System SomeException
  | Command Driver.CommandError
  | Execution Stg.StgException
  | Multiple [EucalyptError]
  deriving (Show, Typeable)

instance Exception EucalyptError

instance Reportable EucalyptError where
  code (Syntax e) = code e
  code (Source e) = code e
  code _ = Nothing

  report (Syntax e) = report e
  report (Core e) = report e
  report (Source e) = report e
  report (Command e) = report e
  report (Execution e) = report e
  report e = P.text $ show e


-- | We can add a source map to a 'EucalyptError' to enhance code
-- reporting.
instance Reportable (SourceMap, EucalyptError) where
  code (_, Syntax e) = code e
  code (_, Source e) = code e
  code (_, System _) = Nothing
  code (_, Command e) = code e
  code (sm, Core e) = code e <|> toSource sm e
  code (sm, Execution e) = code e <|> toSource sm e
  code _ = Nothing

  report (_, e) = report e

  callTrace (sm, Execution Stg.StgException {..}) =
    Just $ CS.resolveSMIDs stgExcCallStack sm
  callTrace _ = Nothing


flattenErrors :: EucalyptError -> [EucalyptError]
flattenErrors (Multiple es) = es
flattenErrors e = [e]

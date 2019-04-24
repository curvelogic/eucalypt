{-|
Module      : Eucalypt.Render.Error
Description : Render errors
Copyright   : (c) Greg Hawkins, 2019
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Render.Error where

import Control.Exception.Safe
import Eucalypt.Reporting.Common
import Eucalypt.Reporting.Classes
import Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as P

-- | An issue with a eucalypt document that impedes rendering to a
-- given format
data RenderProblem = HeterogeneousArray | NonTextualKey
  deriving (Eq, Show, Typeable)

instance Reportable RenderProblem where
  report HeterogeneousArray = P.text "a heterogeneous array"
  report NonTextualKey = P.text "a non-textual key"

-- | An error during rendering
data RenderError =
  Unrenderable String
               RenderProblem
  deriving (Show, Eq, Typeable)

instance Exception RenderError

instance Reportable RenderError where
  report (Unrenderable f prob) =
    title "RENDER ERROR" P.$$ P.text "Could not render to" <+>
    P.text f <+> P.text "because of" <+> (report prob <> P.text ".")

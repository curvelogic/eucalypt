{-|
Module      : Eucalypt.Stg.Type
Description : STG machine's notion of type for error reporting
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Type where

import Data.List (intercalate)
import Data.Typeable
import Eucalypt.Stg.Syn (Tag)

data StgType
  = TypeAny
  | TypeNative
  | TypeString
  | TypeNumber
  | TypeSymbol
  | TypeDynamic (Maybe TypeRep)
  | TypeHeapObj
  | TypeBlackHole
  | TypeClosure
  | TypePartialApplication
  | TypeData [Tag]
  deriving (Show, Eq)

shortName :: StgType -> String
shortName TypeAny = "."
shortName TypeNative = "*"
shortName TypeString = "\"\""
shortName TypeNumber = "#"
shortName TypeSymbol = ":"
shortName (TypeDynamic _) = "?"
shortName TypeHeapObj = "@"
shortName TypeBlackHole = "•"
shortName TypeClosure = "λ"
shortName TypePartialApplication = "(_)"
shortName (TypeData t) = show t

longName :: StgType -> String
longName TypeAny = "Any"
longName TypeNative = "Native"
longName TypeString = "String"
longName TypeNumber = "Number"
longName TypeSymbol = "Symbol"
longName (TypeDynamic r) = maybe "Dynamic" show r
longName TypeHeapObj = "HeapObject"
longName TypeBlackHole = "BlackHole"
longName TypeClosure = "Closure"
longName TypePartialApplication = "PartialApplication"
longName (TypeData t) = "DataType(" ++ show t ++ ")"

friendlySignature :: [StgType] -> String
friendlySignature ts = "(" ++ intercalate "," (map longName ts) ++ ")"

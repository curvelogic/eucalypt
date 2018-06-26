module Eucalypt.Core.BuiltinSpec
  ( main
  , spec
  ) where

import Data.Either (fromRight, isLeft)
import Eucalypt.Core.Builtin
import Eucalypt.Core.Error
import Eucalypt.Core.EvalByName
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

right :: Either l r -> r
right = fromRight undefined

runRightInterpreter :: Interpreter a -> a
runRightInterpreter = right . runInterpreter


-- | Evaluate a zero arity builtin
shallowEvalBuiltin :: CoreBuiltinName -> Either EvaluationError CoreExpr
shallowEvalBuiltin name =
  runInterpreter
    (case lookupBuiltin name of
       Just (_, f) -> f return []
       Nothing -> throwEvalError (BuiltinNotFound name (CoreExpShow (bif ("__" ++ name) :: CoreExpr))))


-- | A panic to use in contexts which should not be evaluated
abort :: CoreExpr
abort = app (bif "PANIC") [str "Should not have been evaluated"]


spec :: Spec
spec = do
  describe "Builtin lookup" $ do
    it "has null" $ (right . shallowEvalBuiltin) "NULL" `shouldBe` corenull
    it "has true" $ (right . shallowEvalBuiltin) "TRUE" `shouldBe` corebool True
    it "has false" $
      (right . shallowEvalBuiltin) "FALSE" `shouldBe` corebool False
    it "fails sensibly" $
      shallowEvalBuiltin "NONESUCH" `shouldSatisfy` isLeft
  describe "If builtin" $ do
    it "evaluates false branch lazily" $
      runRightInterpreter (euIf whnfM [corebool True, bif "TRUE", abort]) `shouldBe`
      bif "TRUE"
    it "evaluates true branch lazily" $
      runRightInterpreter (euIf whnfM [corebool False, abort, bif "FALSE"]) `shouldBe`
      bif "FALSE"
    -- it "ingnores recursion traps in unevaluated branch" $
    --   runRightInterpreter
    --     (whnfM
    --        (letexp
    --           [ ( "a"
    --             , (app (bif "IF") [corebool False, var "a", bif "FALSE"]))
    --           ]
    --           (block [element "a" (var "a")]))) `shouldBe`
    --   Right (bif "FALSE")
  describe "List builtins" $ do
    it "extracts head" $
      runRightInterpreter
        (euHead whnfM [CoreList [bif "TRUE", bif "FALSE", bif "FALSE"]]) `shouldBe`
      bif "TRUE"
    it "extracts tail" $
      runRightInterpreter
        (euTail whnfM [CoreList [bif "TRUE", bif "FALSE", bif "FALSE"]]) `shouldBe`
      CoreList [bif "FALSE", bif "FALSE"]
    it "throws when head-ing []" $
      runInterpreter (euHead whnfM [CoreList []]) `shouldSatisfy` isLeft
    it "throws when tail-ing []" $
      runInterpreter (euTail whnfM [CoreList []]) `shouldSatisfy` isLeft
    it "conses" $
      runRightInterpreter (euCons whnfM [int 0, CoreList [int 1, int 2, int 3]]) `shouldBe`
      CoreList [int 0, int 1, int 2, int 3]
    it "conses with empty" $
      runRightInterpreter (euCons whnfM [int 0, CoreList []]) `shouldBe`
      CoreList [int 0]
    it "throws consing a non-list tail" $
      runInterpreter (euCons whnfM [int 0, int 1]) `shouldSatisfy` isLeft
    it "conses with list-valued call" $
      runRightInterpreter
        (euCons
           whnfM
           [int 0, app (bif "TAIL") [CoreList [int 1, int 2, int 3]]]) `shouldBe`
      CoreList [int 0, int 2, int 3]
    it "judges head([1,2,3])=1" $
      runRightInterpreter
        (whnfM (app (bif "HEAD") [CoreList [int 1, int 2, int 3]])) `shouldBe`
      int 1
    it "judges head(cons(h,t))=h" $
      runRightInterpreter
        (whnfM
           (app
              (bif "EQ")
              [ app
                  (bif "HEAD")
                  [app (bif "CONS") [int 1, CoreList [int 2, int 3]]]
              , int 1
              ])) `shouldBe`
      corebool True
    it "judges head(cons(head(l), tail(l)))=head(l)" $
      runRightInterpreter
        (whnfM
           (app
              (bif "HEAD")
              [ app
                  (bif "CONS")
                  [ app (bif "HEAD") [CoreList [int 1, int 2, int 3]]
                  , app (bif "TAIL") [CoreList [int 1, int 2, int 3]]
                  ]
              ])) `shouldBe`
      int 1
    it "judges cons(head(l), tail(l))=l" $
      runRightInterpreter
        (forceDataStructures
           whnfM
           (app
              (bif "CONS")
              [ app (bif "HEAD") [CoreList [int 1, int 2, int 3]]
              , app (bif "TAIL") [CoreList [int 1, int 2, int 3]]
              ])) `shouldBe`
      CoreList [int 1, int 2, int 3]
    it "judges eq(l,(cons(head(l), tail(l))))=true" $
      runRightInterpreter
        (whnfM
           (app
              (bif "EQ")
              [ CoreList [int 1, int 2, int 3]
              , app
                  (bif "CONS")
                  [ app (bif "HEAD") [CoreList [int 1, int 2, int 3]]
                  , app (bif "TAIL") [CoreList [int 1, int 2, int 3]]
                  ]
              ])) `shouldBe`
      corebool True
  arithSpec
  booleanSpec
  blockSpec
  stringSpec


addsIntegers :: Integer -> Integer -> Bool
addsIntegers l r = runRightInterpreter (euAdd whnfM [int l, int r]) == int (l + r)

addsFloats :: Double -> Double -> Bool
addsFloats l r = runRightInterpreter (euAdd whnfM [float l, float r]) == float (l + r)

subtractsIntegers :: Integer -> Integer -> Bool
subtractsIntegers l r = runRightInterpreter (euSub whnfM [int l, int r]) == int (l - r)

subtractsFloats :: Double -> Double -> Bool
subtractsFloats l r = runRightInterpreter (euSub whnfM [float l, float r]) == float (l - r)

multipliesIntegers :: Integer -> Integer -> Bool
multipliesIntegers l r = runRightInterpreter (euMul whnfM [int l, int r]) == int (l * r)

multipliesFloats :: Double -> Double -> Bool
multipliesFloats l r = runRightInterpreter (euMul whnfM [float l, float r]) == float (l * r)

dividesIntegers :: Integer -> NonZero Integer -> Bool
dividesIntegers l (NonZero r) = runRightInterpreter (euDiv whnfM [int l, int r]) == float (fromInteger l / fromInteger r)

dividesFloats :: Double -> NonZero Double -> Bool
dividesFloats l (NonZero r) = runRightInterpreter (euDiv whnfM [float l, float r]) == float (l / r)

ordersIntegersWithLt :: Integer -> Integer -> Bool
ordersIntegersWithLt l r = runRightInterpreter (euLt whnfM [int l, int r]) == corebool True
  || runRightInterpreter (euGte whnfM [int l, int r]) == corebool True

ordersIntegersWithGt :: Integer -> Integer -> Bool
ordersIntegersWithGt l r = runRightInterpreter (euLte whnfM [int l, int r]) == corebool True
  || runRightInterpreter (euGt whnfM [int l, int r]) == corebool True

ordersFloatsWithLt :: Double -> Double -> Bool
ordersFloatsWithLt l r = runRightInterpreter (euLt whnfM [float l, float r]) == corebool True
  || runRightInterpreter (euGte whnfM [float l, float r]) == corebool True

ordersFloatsWithGt :: Double -> Double -> Bool
ordersFloatsWithGt l r = runRightInterpreter (euLte whnfM [float l, float r]) == corebool True
  || runRightInterpreter (euGt whnfM [float l, float r]) == corebool True

equatesEqualInts :: Integer -> Bool
equatesEqualInts n = runRightInterpreter (euEq whnfM [int n, int n]) == corebool True

equatesEqualFloats :: Double -> Bool
equatesEqualFloats n = runRightInterpreter (euEq whnfM [float n, float n]) == corebool True


arithSpec :: Spec
arithSpec =
  describe "Arithmetic operations" $ do
    it "adds ints" $ property addsIntegers
    it "subtracts ints" $ property subtractsIntegers
    it "multiplies ints" $ property multipliesIntegers
    it "adds floats" $ property addsFloats
    it "subtracts floats" $ property subtractsFloats
    it "multiplies floats" $ property multipliesFloats
    it "divides integers" $ property dividesIntegers
    it "divides floats" $ property dividesFloats
    it "orders ints with < / >=" $ property ordersIntegersWithLt
    it "orders ints with <= / >" $ property ordersIntegersWithGt
    it "orders floats with < / >=" $ property ordersFloatsWithLt
    it "orders floats with <= / >" $ property ordersFloatsWithGt
    it "equates equal ints" $ property equatesEqualInts
    it "equates equal floats" $ property equatesEqualFloats


calculatesAnd :: Bool -> Bool -> Bool
calculatesAnd l r =
  runRightInterpreter (euAnd return [corebool l, corebool r]) ==
  corebool (l && r)

calculatesOr :: Bool -> Bool -> Bool
calculatesOr l r =
  runRightInterpreter (euOr return [corebool l, corebool r]) ==
  corebool (l || r)

calculatesNot :: Bool -> Bool
calculatesNot b =
  runRightInterpreter (euNot return [corebool b]) == corebool (not b)

booleanSpec :: Spec
booleanSpec =
  describe "Boolean operators" $ do
    it "calculates and" $ property calculatesAnd
    it "calculates or" $ property calculatesOr
    it "calculates not" $ property calculatesNot


blockSpec :: Spec
blockSpec =
  describe "Block merges" $
    it "overrides preserving original order" $
      let b1 = block [element "b" $ int 1, element "a" $ int 5]
          b2 = block [element "b" $ int (-1)]
          b3 = block [element "b" $ int (-1), element "a" $ int 5]
          merged = runRightInterpreter $ euMerge return [b1, b2]
      in (merged `shouldBe` b3)


stringSpec :: Spec
stringSpec = do
  describe "euSplit" $ do
    it "splits on regex" $
      runRightInterpreter (euSplit return [str "1.2.3.4", str "\\."])
      `shouldBe`
      CoreList [str "1", str "2", str "3", str "4"]
    it "filters out empty strings" $
      runRightInterpreter (euSplit return [str "1..2", str "\\."])
      `shouldBe`
      CoreList [str "1", str "2"]
    it "handles empty regex" $
      runRightInterpreter (euSplit return [str "foo..bar", str ""])
      `shouldBe`
      CoreList [str "foo..bar"]
    it "handles dot regex" $
      runRightInterpreter (euSplit return [str "foo", str "."])
      `shouldBe`
      CoreList []
    it "handles .* regex" $
      runRightInterpreter (euSplit return [str "foo", str ".*"])
      `shouldBe`
      CoreList []
  describe "euJoin" $
    it "join on separator" $
      runRightInterpreter (euJoin return [CoreList [str "1", str "2", str "3"], str "-"])
      `shouldBe`
      str "1-2-3"
  describe "euMatch" $
    it "matches regex" $
      runRightInterpreter (euMatch return [str "192.168.0.2", str "(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)"])
      `shouldBe`
      CoreList [str "192.168.0.2", str "192", str "168", str "0", str "2"]
  describe "euMatches" $
    it "searches with regex" $
      runRightInterpreter (euMatches return [str "192.168.0.2", str "(\\d+)"])
      `shouldBe`
      CoreList [str "192", str "168", str "0", str "2"]

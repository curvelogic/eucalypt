{-# LANGUAGE LambdaCase #-}

module Eucalypt.Core.DesugarSpec
  ( main
  , spec
  ) where

import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, fromJust, maybeToList)
import Eucalypt.Core.Desugar
import Eucalypt.Core.Import
import Eucalypt.Core.Metadata
import Eucalypt.Core.Target
import qualified Eucalypt.Core.AnonSyn as ASyn
import qualified Eucalypt.Core.Syn as Syn
import Eucalypt.Core.Unit
import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.ParseExpr
import Eucalypt.Syntax.Input
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  coreSpec
  soupSpec
  blockSpec
  sampleSpec
  targetsSpec
  importsSpec
  interpolationSpec

inputsFromMetadata :: Syn.CoreExp a -> [Input]
inputsFromMetadata m = fromMaybe [] $ readUnevaluatedMetadata "import" m extract
  where
    extract (Syn.CorePrim _ (Syn.CoreString s)) = maybeToList $ parseInputFromString s
    extract (Syn.CoreList _ l) = concatMap extract l
    extract _ = []

-- | An import handler for testing that reads simple input imports but
-- doesn't handle git imports.
testImportHandler :: ImportHandler
testImportHandler =
  ImportHandler
    { readImports = \m -> zip (inputsFromMetadata m) (repeat $ return ())
    , pruneImports = pruneUnevaluatedMetadata "import"
    }


fakeInput :: Input
fakeInput =
  Input {inputLocator = StdInput, inputName = Nothing, inputFormat = "eu"}

-- ? shims
testDesugarSoup :: [Expression] -> Syn.CoreExpr
testDesugarSoup =
  (`evalState` initTranslateState 1 nullImportHandler) .
  unTranslate . translateSoup

testDesugarBlock :: Block -> Syn.CoreExpr
testDesugarBlock =
  (`evalState` initTranslateState 1 nullImportHandler) .
  unTranslate . translateBlock nowhere

testDesugar :: Expression -> Syn.CoreExpr
testDesugar =
  (`evalState` initTranslateState 1 nullImportHandler) . unTranslate . translate

testDesugarLiteral :: PrimitiveLiteral -> Syn.CoreExpr
testDesugarLiteral =
  (`evalState` initTranslateState 1 nullImportHandler) .
  unTranslate . desugarLiteral nowhere

coreSpec :: Spec
coreSpec =
  describe "Core" $ do
    it "represents literals" $
      testDesugarLiteral (VInt 8) `shouldBe` Syn.int 1 8
    it "processes annotation shortcuts" $
      normaliseMetadata (ASyn.str "blah") `shouldBe`
      ASyn.block [ASyn.element "doc" $ ASyn.str "blah"]
    xit "shifts declaration metadata" $
      Syn.unbind
        (testDesugarBlock
           (at nowhere $ Block [ann (str "docs") (prop "x" (int 5))])) `shouldSatisfy` \case
        Syn.CoreBlock _ (Syn.CoreList _ [Syn.CoreMeta{}]) -> True
        _ -> False


soupSpec :: Spec
soupSpec =
  describe "soup desugaring" $ do
    it "inserts call operators" $
      testDesugarSoup
        [int 5, normalName "x", normalName "f", applyTuple [int 4, int 7]] `shouldBe`
      ASyn.soup
        [ Syn.int 1 5
        , Syn.var 2 "x"
        , Syn.var 3 "f"
        , Syn.callOp
        , Syn.args 6 [Syn.int 4 4, Syn.int 5 7]
        ]
    it "handles iterated calls" $
      testDesugarSoup
        [ normalName "f"
        , applyTuple [normalName "x"]
        , applyTuple [normalName "y"]
        ] `shouldBe`
      ASyn.soup
        [ Syn.var 1 "f"
        , Syn.callOp
        , Syn.args 3 [Syn.var 2 "x"]
        , Syn.callOp
        , Syn.args 5 [Syn.var 4 "y"]
        ]
    it "handles relative names" $
      testDesugarSoup
        [ normalName "x"
        , operatorName "."
        , normalName "y"
        , operatorName "."
        , normalName "z"
        ] `shouldBe`
      ASyn.soup
        [ Syn.var 1 "x"
        , Syn.lookupOp
        , Syn.corename 2 "y"
        , Syn.lookupOp
        , Syn.corename 3 "z"
        ]

blockSpec :: Spec
blockSpec =
  describe "block desugaring" $ do
    it "creates vars for lonely names" $
      testDesugarBlock (at nowhere $ Block [bare (prop "x" (normalName "y"))]) `shouldBe`
      Syn.letblock
        4
        [("x", Syn.var 2 "y")]
        (Syn.block 3 [ASyn.element "x" $ ASyn.var "x"])
    it "handles built-ins" $
      testDesugarBlock
        (at nowhere $
         Block
           [ bare (prop "null" (normalName "__NULL"))
           , bare (prop "a" (normalName "null"))
           ]) `shouldBe`
      Syn.letblock
        6
        [("null", Syn.bif 2 "NULL"), ("a", Syn.var 4 "null")]
        (Syn.block
           5
           [ Syn.element 0 "null" $ ASyn.var "null"
           , Syn.element 0 "a" $ ASyn.var "a"
           ])

sampleSpec :: Spec
sampleSpec =
  describe "samples" $ do
    it "desugars eq(or(f, and(t, t)), t)" $
      testDesugar <$>
      parseExpression "eq(or(f, and(t, t)), t)" "test" `shouldBe`
      Right
        (ASyn.soup
           [ Syn.var 1 "eq"
           , Syn.callOp
           , Syn.args
               10
               [ ASyn.soup
                   [ Syn.var 2 "or"
                   , Syn.callOp
                   , Syn.args
                       8
                       [ Syn.var 3 "f"
                       , ASyn.soup
                           [ Syn.var 4 "and"
                           , Syn.callOp
                           , Syn.args 7 [Syn.var 5 "t", Syn.var 6 "t"]
                           ]
                       ]
                   ]
               , Syn.var 9 "t"
               ]
           ])
    it "desugars __HEAD(__CONS([1, 2, 3] __HEAD, [1, 2, 3] __TAIL))" $
      testDesugar <$>
      parseExpression
        "__HEAD(__CONS([1, 2, 3] __HEAD, [1, 2, 3] __TAIL))"
        "test" `shouldBe`
      Right
        (ASyn.soup
           [ Syn.bif 1 "HEAD"
           , Syn.callOp
           , Syn.args
               14
               [ ASyn.soup
                   [ Syn.bif 2 "CONS"
                   , Syn.callOp
                   , Syn.args
                       13
                       [ ASyn.soup
                           [ Syn.corelist
                               6
                               [Syn.int 3 1, Syn.int 4 2, Syn.int 5 3]
                           , Syn.bif 7 "HEAD"
                           ]
                       , ASyn.soup
                           [ Syn.corelist
                               11
                               [Syn.int 8 1, Syn.int 9 2, Syn.int 10 3]
                           , Syn.bif 12 "TAIL"
                           ]
                       ]
                   ]
               ]
           ])
    it "desugars x - 1" $
      testDesugar <$>
      parseExpression "x - 1" "test" `shouldBe`
      Right (ASyn.soup [Syn.var 1 "x", Syn.var 2 "-", Syn.int 3 1])
    it "desugars f(x).v" $
      testDesugar <$>
      parseExpression "f(x).v" "test" `shouldBe`
      Right
        (ASyn.soup
           [ Syn.var 1 "f"
           , Syn.callOp
           , Syn.args 3 [Syn.var 2 "x"]
           , Syn.lookupOp
           , Syn.corename 4 "v"
           ])


targetAnnotation :: String -> String -> Expression
targetAnnotation n d = block [bare $ prop "target" $ sym n , bare $ prop "doc" $ str d]

targetFormatAnnotation :: String -> String -> String -> Expression
targetFormatAnnotation n d f =
  block
    [ bare $ prop "target" $ sym n
    , bare $ prop "doc" $ str d
    , bare $ prop "format" $ sym f
    ]

targetSampleA :: Unit
targetSampleA =
  bareUnit
    [ bare $
      prop "a" $ block [ann (targetAnnotation "T" "x") (prop "b" $ int 1)]
    ]

targetSampleB :: Unit
targetSampleB =
  bareUnit
    [ bare $
      prop "a" $
      block
        [ ann (targetAnnotation "T" "x") (prop "b" $ int 1)
        , ann (targetAnnotation "U" "y") (prop "c" $ int 1)
        ]
    ]

targetsSpec :: Spec
targetsSpec =
  describe "target detection" $ do
    it "reads annotation ` {target: :T doc: \"x\"}" $
      (determineTarget . testDesugar) (targetAnnotation "T" "x") `shouldBe`
      Just ("T", "x", Nothing)
    it "finds T in { a: { ` {target: :T doc: \"x\"} b: _ } }" $
      (truTargets . translateToCore testImportHandler fakeInput 1) targetSampleA `shouldBe`
      [TargetSpec "T" "x" Nothing ["a", "b"]]
    it "finds T and U in larger sample " $
      (truTargets . translateToCore testImportHandler fakeInput 1) targetSampleB `shouldBe`
      [TargetSpec "T" "x" Nothing ["a", "b"], TargetSpec "U" "y" Nothing ["a", "c"]]
    it "reads annotation ` {target: :T format: :f doc: \"x\"}" $
      (determineTarget . testDesugar) (targetFormatAnnotation "T" "x" "f") `shouldBe`
      Just ("T", "x", Just "f")

importAnnotation :: [String] -> Expression
importAnnotation inputs = block [bare $ prop "import" $ list (map str inputs)]

importSampleA :: Unit
importSampleA =
  bareUnit
    [ bare $
      prop "a" $
      block [ann (importAnnotation ["a.yaml", "b.yaml"]) (prop "b" $ int 1)]
    ]

importsSpec :: Spec
importsSpec =
  describe "import detection" $
  -- TODO: move to driver.core
  -- it "reads annotation `{import: [\"x.eu\", \"y.eu\"]}`" $
  --   (importsFromMetadata . testDesugar) (importAnnotation ["x.eu", "y.eu"]) `shouldBe`
  --   traverse parseInputFromString ["x.eu", "y.eu"]
  it "finds imports for nested block" $
  (toList . truImports . translateToCore testImportHandler fakeInput 1)
    importSampleA `shouldBe`
  fromJust (traverse parseInputFromString ["a.yaml", "b.yaml"])

interpolationSpec:: Spec
interpolationSpec =
  describe "string interpolation" $ do
    it "decomposes pattern \"{foo}bar\"" $
      testDesugar <$>
      parseExpression "\"{foo}bar\"" "test" `shouldBe`
      Right
        (Syn.app
           1
           (ASyn.bif "JOIN")
           [ Syn.corelist
               1
               [ASyn.app (ASyn.bif "STR") [ASyn.var "foo"], ASyn.str "bar"]
           , ASyn.str ""
           ])
    it "decomposes pattern \"{foo}bar\"" $
      testDesugar <$>
      parseExpression "\"foo{bar}\"" "test" `shouldBe`
      Right
        (Syn.app
           1
           (ASyn.bif "JOIN")
           [ Syn.corelist
               1
               [ASyn.str "foo", ASyn.app (ASyn.bif "STR") [ASyn.var "bar"]]
           , ASyn.str ""
           ])
    it "decomposes pattern \"{foo}{bar}\"" $
      testDesugar <$>
      parseExpression "\"{foo}{bar}\"" "test" `shouldBe`
      Right
        (Syn.app
           1
           (ASyn.bif "JOIN")
           [ Syn.corelist
               1
               [ ASyn.app (ASyn.bif "STR") [ASyn.var "foo"]
               , ASyn.app (ASyn.bif "STR") [ASyn.var "bar"]
               ]
           , ASyn.str ""
           ])
    it "handles printf format specifiers \"{foo:%-3d}{bar:%-3d}\"" $
      testDesugar <$>
      parseExpression "\"{foo:%-3d}{bar:%-3d}\"" "test" `shouldBe`
      Right
        (Syn.app
           1
           (ASyn.bif "JOIN")
           [ Syn.corelist
               1
               [ASyn.app (ASyn.bif "FMT") [ASyn.var "foo", ASyn.str "%-3d"],
                ASyn.app (ASyn.bif "FMT") [ASyn.var "bar", ASyn.str "%-3d"]]
           , ASyn.str ""
           ])
    it "handles fn format specifiers \"{foo:myfmt}{bar:myfmt}\"" $
      testDesugar <$>
      parseExpression "\"{foo:myfmt}{bar:myfmt}\"" "test" `shouldBe`
      Right
        (Syn.app
           1
           (ASyn.bif "JOIN")
           [ Syn.corelist
               1
               [ASyn.app (ASyn.var "myfmt") [ASyn.var "foo"],
                ASyn.app (ASyn.var "myfmt") [ASyn.var "bar"]]
           , ASyn.str ""
           ])
    it "handles empty string \"\"" $
      testDesugar <$>
      parseExpression "\"\"" "test" `shouldBe` Right (Syn.str 1 "")
    it "desugars numbered anaphora" $
      testDesugar <$>
      parseExpression "\"{0}{1}\"" "test" `shouldBe`
      Right
        (Syn.lam
           1
           ["_0", "_1"]
           (Syn.app
              1
              (ASyn.bif "JOIN")
              [ Syn.corelist
                  1
                  [ ASyn.app (ASyn.bif "STR") [ASyn.var "_0"]
                  , ASyn.app (ASyn.bif "STR") [ASyn.var "_1"]
                  ]
              , ASyn.str ""
              ]))
    it "desugars unnumbered anaphora" $
      testDesugar <$>
      parseExpression "\"{}{}\"" "test" `shouldBe`
      Right
        (Syn.lam
           1
           ["_0", "_1"]
           (Syn.app
              1
              (ASyn.bif "JOIN")
              [ Syn.corelist
                  1
                  [ ASyn.app (ASyn.bif "STR") [ASyn.var "_0"]
                  , ASyn.app (ASyn.bif "STR") [ASyn.var "_1"]
                  ]
              , ASyn.str ""
              ]))
    it "desugars unnumbered anaphora with free vars" $
      testDesugar <$>
      parseExpression "\"{foo}{}\"" "test" `shouldBe`
      Right
        (Syn.lam
           1
           ["_0"]
           (Syn.app
              1
              (ASyn.bif "JOIN")
              [ Syn.corelist
                  1
                  [ ASyn.app (ASyn.bif "STR") [ASyn.var "foo"]
                  , ASyn.app (ASyn.bif "STR") [ASyn.var "_0"]
                  ]
              , ASyn.str ""
              ]))
    it "desugars both anaphora with free vars" $
      testDesugar <$>
      parseExpression "\"{foo}{1}{}\"" "test" `shouldBe`
      Right
        (Syn.lam
           1
           ["_0", "_1"]
           (Syn.app
              1
              (ASyn.bif "JOIN")
              [ Syn.corelist
                  1
                  [ ASyn.app (ASyn.bif "STR") [ASyn.var "foo"]
                  , ASyn.app (ASyn.bif "STR") [ASyn.var "_1"]
                  , ASyn.app (ASyn.bif "STR") [ASyn.var "_0"]
                  ]
              , ASyn.str ""
              ]))

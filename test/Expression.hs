{-# LANGUAGE OverloadedStrings #-}

import Data.Function((&))
import Data.Text(Text)
import Test.Hspec

import Model.Expression
import Model.Expression.Manipulate
import Model.Expression.Lexer
import Model.Expression.Parser
import Model.Expression.Evaluation
import Model.RowStore
import Model.RowStore.RowStoreConf
import Model.SourceInfo(FormatInfo(..))

mainRst :: RowStore
mainRst = emptyConf "main" conf & addRowStore childRst
  where conf = fromFieldConf [
                 FieldConf (Just "first") TypeInt Nothing
               , FieldConf (Just "second") TypeInt Nothing
               , FieldConf (Just "third") TypeString Nothing
               ]


formulaRst :: RowStore
formulaRst = emptyConf "main" conf
  where conf = fromFieldConf [
                  FieldConf (Just "first") TypeInt Nothing
                , FieldConf (Just "formula") TypeString (Just "name @ child <- first <-> value")
                ]

-- Convenience functions
shouldBeF :: ToField f => Field -> f -> Expectation
shouldBeF l r = l `shouldBe` toField r

shouldBeI :: Field -> Int -> Expectation
shouldBeI = shouldBeF


childRst :: RowStore
childRst = emptyConf "child" conf & ins "one" 1 & ins "two" 2
  where conf = fromFieldConf [
                 FieldConf (Just "initial") TypeString Nothing
               , FieldConf (Just "name") TypeString Nothing
               , FieldConf (Just "value") TypeInt Nothing
               , FieldConf (Just "other") TypeInt Nothing
               ]
        ins :: Text -> Int -> RowStore -> RowStore 
        ins n v rst = addRow rst [toField ("Initial" :: Text),
                                  toField n, toField v, toField ("Other" :: Text)]




simpleEval :: [Field] -> Formula -> Field
simpleEval fs = evaluate fs [] . parse


testSimpleExpressions :: Spec
testSimpleExpressions = describe "Test simple expressions" $ do
                     it "Adds 2 and 2" $
                       simpleEval [] "2 + 2" `shouldBeI` 4

                     it "Multiplies 2 by 3" $
                       simpleEval [] "2 * 3" `shouldBeI` 6

evalInts :: [Int] -> Expression -> Field
evalInts xs = let
    fs = map toField xs
  in evaluate fs []

evalNoNames :: [Int] -> Text -> Field
evalNoNames xs = evalInts xs . parse

testAbsolutePositions :: Spec
testAbsolutePositions = describe "Test absolute positions" $
                     it "Checks absolute positions" $
                       evalNoNames [1, 2, 3] "$1 + $2" `shouldBeI` 3

evalNames :: [Int] -> RowStore -> Text -> Field
evalNames xs rst = let
    fs = map toField xs
    dss = getDataSources rst
  in evaluate fs dss . eliminateNames mainRst . parse

testNames :: Spec
testNames = describe "Test names" $ do
                        it "Simple arithmetic with names" $ do
                          evalNames [3, 2] mainRst "first - second" `shouldBeI` 1
                          evalNames [6, 3] mainRst "@{first} / @{second}" `shouldBeF` (2 :: Double)
                        it "Checks the substitution of names" $ do
                          eliminateNames mainRst (parse "first") `shouldBe` mkPosition 0
                          eliminateNames mainRst (parse "second") `shouldBe` mkPosition 1
                          eliminateNames mainRst (parse "other @ child <- second <-> value")
                             `shouldBe` mkFromSource (mkConstant $ toField (0 :: Int))
                                                     (mkPosition 1)
                                                     (mkPosition 2)
                                                     (mkPosition 3)
                          eliminateNames mainRst (parse "first + value @ child <- \"one\" <-> name")
                             `shouldBe` mkBinary (BinaryOpInfo (+) "+" 4 TrueAssoc)
                                                 (mkPosition 0)
                                                 (mkFromSource (mkConstant $ toField (0 :: Int))
                                                               (mkConstant $ toField ("one" :: Text))
                                                               (mkPosition 1)
                                                               (mkPosition 2)
                                                 )
                                                                                                                                    
                                                                               

testSearch :: Spec
testSearch = describe "Test search" $ do
                        it "Simple arithmetic" $ do
                          evalNames [10, 20] mainRst "first + value @ child <- \"one\" <-> name" `shouldBeI` 11
                          evalNames [10, 20] mainRst "first + value @ child <- \"two\" <-> name" `shouldBeI` 12
                        it "Adding a source must recompute" $ do
                          let rst = addRow formulaRst [1, 20]
                              rst' = addRowStore childRst rst
                          row 0 rst' !! 1 `shouldBeF` ("one" :: Text)


testLexer :: Spec
testLexer = describe "Test the lexer" $ do
              it "Checks the @ symbol" $
                tokenize "@ @{name}" `shouldBe` [AtT, NameT "name", EOFT]
              it "Checks the < symbol" $
                tokenize "< <= <- <->" `shouldBe` [LessThanT, LessOrEqualT, ArrowT, DoubleArrowT, EOFT]

testParser :: Spec
testParser = describe "Test the parser" $
               it "Checks a from source expression" $
                 parse "value @ child <- name <-> name2" `shouldBe` mkFromSource (mkNamedPosition "child")
                                                                                (mkNamedPosition "name")
                                                                                (mkNamedPosition "name2")
                                                                                (mkNamedPosition "value")


main:: IO ()
main = hspec $ do
  testSimpleExpressions
  testAbsolutePositions
  testNames
  testSearch
  testLexer
  testParser
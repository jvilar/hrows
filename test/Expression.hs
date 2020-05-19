{-# LANGUAGE OverloadedStrings #-}

import Data.Text(Text)
import Test.Hspec

import Model.Expression
import Model.Expression.Manipulate
import Model.Expression.Lexer
import Model.Expression.Parser
import Model.Expression.Evaluation
import Model.RowStore
import Model.RowStoreConf

mainRst :: RowStore
mainRst = emptyConf "main" conf & addRowStore childRst
  where conf = RowStoreConf [
                 FieldConf (Just "first") TypeInt Nothing
               , FieldConf (Just "second") TypeInt Nothing
               ]


-- Convenience functions
(&) = flip id

shouldBeF :: ToField f => Field -> f -> Expectation
shouldBeF l r = l `shouldBe` toField r

shouldBeI :: Field -> Int -> Expectation
shouldBeI = shouldBeF


childRst :: RowStore
childRst = emptyConf "child" conf & ins "one" 1 & ins "two" 2
  where conf = RowStoreConf [
                 FieldConf (Just "name") TypeString Nothing
               , FieldConf (Just "value") TypeInt Nothing
               ]
        ins :: Text -> Int -> RowStore -> RowStore 
        ins n v rst = addRow rst [toField n, toField v]




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
evalNames xs rst = evalInts xs . eliminateNames rst . parse


testNames :: Spec
testNames = describe "Test names" $ do
                        it "Subtracts 2 from 3" $
                          evalNames [3, 2] mainRst "first - second" `shouldBeI` 1
                        it "Divides 6 by 3 (long names)" $
                          evalNames [6, 3] mainRst "@{first} / @{second}" `shouldBeF` (2 :: Double)

testSearch :: Spec
testSearch = describe "Test search" $ do
                        it "Adds one plus one" $
                          evalNames [1, 2] mainRst "first + value @ child <- name <-> \"one\"" `shouldBeI` 2
                        it "Adds one plus two" $
                          evalNames [1, 2] mainRst "first + value @ child <- name <-> \"two\"" `shouldBeI` 3


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
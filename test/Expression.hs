{-# LANGUAGE OverloadedStrings #-}

import Data.Text(Text)
import Test.Hspec

import Model.Expression
import Model.Expression.Parser
import Model.Expression.Evaluation
import Model.RowStore

rst = empty ""

simpleEval :: [Field] -> Text -> Field
simpleEval fs f = let
    e = parse f
  in evaluate fs [] e

evalInt :: [Int] -> Text -> Int
evalInt xs f = let
     e = parse f
     fs = map toField xs
   in toInt $ evaluate fs [] e

main:: IO ()
main = hspec $
  describe "prueba" $ do
    it "Adds 2 and 2" $
      simpleEval [] "2 + 2" `shouldBe` toField (4 :: Int)
      
    it "Multiply 2 by 3" $
      simpleEval [] "2 * 3" `shouldBe` toField (6 :: Int)

    it "Check absolute positions" $
      evalInt [1, 2, 3] "$1 + $2" `shouldBe` 3
      
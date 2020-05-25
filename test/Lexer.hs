{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Model.Expression.Lexer
import Model.Field

testNumbers :: Spec
testNumbers = describe "Numeric tokens" $ do
  it "Test the integers" $ do
      tokenize "0" `shouldBe` [IntT 0, EOFT]
      tokenize "1" `shouldBe` [IntT 1, EOFT]
      tokenize "234" `shouldBe` [IntT 234, EOFT]
      tokenize "567 890" `shouldBe` [IntT 567, IntT 890, EOFT]
  it "Test the doubles" $ do
      tokenize "1.2" `shouldBe` [DoubleT 1.2, EOFT]
      tokenize "1.2e3" `shouldBe` [DoubleT 1.2e3, EOFT]
      tokenize "1.2e-3" `shouldBe` [DoubleT 1.2e-3, EOFT]
      tokenize "1.2e+3" `shouldBe` [DoubleT 1.2e+3, EOFT]
      tokenize "-1.2" `shouldBe` [SubT, DoubleT 1.2, EOFT]
      tokenize "1." `shouldBe` [IntT 1, ErrorT ".", EOFT]
      tokenize ".1" `shouldBe` [ErrorT ".", IntT 1, EOFT]

testOperators :: Spec
testOperators = describe "Operators" $ do
  it "Arithmetic operators" $ do
      tokenize "+" `shouldBe` [AddT, EOFT]
      tokenize "-" `shouldBe` [SubT, EOFT]
      tokenize "*/" `shouldBe` [MultT, DivT, EOFT]
  it "Parethesis" $
      tokenize "()" `shouldBe` [OpenT, CloseT, EOFT]
  it "Logical operators" $ do
      tokenize "< > <= >= == = !=" `shouldBe` [LessThanT, GreaterThanT, LessOrEqualT, GreaterOrEqualT,
                                               EqualT, EqualT, NotEqualT, EOFT]
      tokenize "?:" `shouldBe` [QuestionMarkT, ColonT, EOFT]
      tokenize "&& || !" `shouldBe` [AndT, OrT, NotT, EOFT]
  it "Searches" $
      tokenize "@ <- <->" `shouldBe` [AtT, ArrowT, DoubleArrowT, EOFT]
  it "Reserved words" $
      tokenize "min max str int int0 float float0" `shouldBe`
           [MinT, MaxT, CastT TypeString, CastT TypeInt, CastT TypeInt0,
            CastT TypeDouble, CastT TypeDouble0, EOFT]

testNames :: Spec
testNames = describe "Names" $ do
  it "Test long names" $ do
    tokenize "@{name}" `shouldBe` [NameT "name", EOFT]
    tokenize "@{strange# name!}" `shouldBe` [NameT "strange# name!", EOFT]
  it "Test short names" $ do
    tokenize "name" `shouldBe` [NameT "name", EOFT]
    tokenize "two names" `shouldBe` [NameT "two", NameT "names", EOFT]

testStrings :: Spec
testStrings = describe "Strings" $
  it "Tests strings" $ do
    tokenize "\"Hello world\"" `shouldBe` [StringT "Hello world", EOFT]
    tokenize "\"A string\"" `shouldBe` [StringT "A string", EOFT]

testPositions :: Spec
testPositions = describe "Positions" $
  it "Tests positions" $
    tokenize "$1 $2 $3" `shouldBe` [PositionT 1, PositionT 2, PositionT 3, EOFT]

main :: IO()
main = hspec $ do
  testNumbers
  testOperators
  testNames
  testStrings
  testPositions

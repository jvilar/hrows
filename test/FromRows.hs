{-# LANGUAGE OverloadedStrings #-}

import Data.Function((&))
import Data.Text (Text)
import Test.Hspec

import Model.RowStore
import Model.RowStore.RowStoreConf

mainRst :: RowStore
mainRst = emptyConf "main" conf
  where conf = fromFieldConf [
                 FieldConf (Just "first") TypeInt Nothing
               , FieldConf (Just "second") TypeInt Nothing
               , FieldConf (Just "third") TypeString Nothing
               ]


simpleRow :: Spec
simpleRow = describe "Adding a row with errors does not add messages" $ do
    it "Keeps the text" $ do
       let texts = [ "one", "two", "three" ] :: [Text]
           errorRow = map toField texts
           rst = addRow mainRst errorRow
           newRow = row 0 rst
       map toString newRow `shouldBe` texts


main :: IO ()
main = hspec simpleRow

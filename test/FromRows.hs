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
    let texts = [ "one", "two", "three" ] :: [Text]
        errorRow = map toField texts
        rst = addRow mainRst errorRow
    it "Keeps the text in add" $ do
       let newRow = row 0 rst
       map toString newRow `shouldBe` texts
    it "Keeps the text in changes" $ do
       let (rst', _) = changeField 0 0 (head errorRow) rst
           newRow = row 0 rst'
       toString (head newRow) `shouldBe` head texts



main :: IO ()
main = hspec simpleRow

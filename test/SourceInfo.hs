{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy(ByteString)
import Data.Either(isRight)
import Data.Maybe(fromJust, isJust)
import Test.Hspec

import Model.SourceInfo

okSourceInfo :: ByteString -> Expectation
-- okSourceInfo bs = isJust (decode bs :: Maybe SourceInfo) `shouldBe` True
okSourceInfo bs = isRight (parseEither parseJSON (fromJust $ decode bs :: Value) :: Either String SourceInfo) `shouldBe` True

testSimpleConfNoListatabFormat :: Spec
testSimpleConfNoListatabFormat = describe "Test a very simple configuration without ListataInfo" $ do
    it "Read a very simple configuration with no format info" $ do
      okSourceInfo "{\"siName\":\"patata\",\"siFilePath\":\"/lib/patata\",\"siConfFile\":null,\"siFormat\":{\"tag\":\"NoFormatInfo\"}}"

testSimpleConfWithListatabFormat :: Spec
testSimpleConfWithListatabFormat = describe "Test a very simple configuration with a ListataInfo" $ do
    it "Read a very simple configuration with listatab format info" $ do
      okSourceInfo "{\"siName\":\"patata\",\"siFilePath\":\"/lib/patata\",\"siConfFile\":null,\"siFormat\":{\"tag\":\"ListatabFormat\",\"contents\":{\"ltInputSeparator\":\"\\t\",\"ltOutputSeparator\":\"\\t\",\"ltHeaderType\":\"Comment\"}}}"

testOldConf :: Spec
testOldConf = describe "Test old sourceInfo" $ do
    it "Reads a sourceInfo without name and null file name" $ do
      okSourceInfo " {\
      \      \"siConfFile\": null,\
      \      \"siFilePath\": \"/home/jvilar/ASIGNATURAS/2021/VJ1229/submissions/names\",\
      \      \"siFormat\": {\
      \          \"tag\": \"ListatabFormat\",\
      \          \"contents\": {\
      \              \"ltInputSeparator\": \"\\t\",\
      \              \"ltHeaderType\": \"Comment\",\
      \              \"ltOutputSeparator\": \"\\t\"\
      \          }\
      \      }\
      \  }"
    it "Reads another old sourceInfo" $ do
      okSourceInfo " {\
      \      \"siConfFile\": null,\
      \      \"siFilePath\": \"/home/jvilar/ASIGNATURAS/2021/EI1017/entregas/P2/notas\",\
      \      \"siFormat\": {\
      \          \"tag\": \"ListatabFormat\",\
      \          \"contents\": {\
      \              \"ltInputSeparator\": \"\\t\",\
      \              \"ltHeaderType\": \"Comment\",\
      \              \"ltOutputSeparator\": \"\\t\"\
      \          }\
      \      }\
      \  }"


testNewConf :: Spec
testNewConf = describe "Test new sourceInfo" $ do
    it "Reads a sourceInfo with the name and non-null file name" $ do
      okSourceInfo " {\
      \      \"siName\": \"numeros\",\
      \      \"siConfFile\": null,\
      \      \"siFilePath\": \"/home/jvilar/APLICACIONES/hrows/examples/numeros\",\
      \      \"siFormat\": {\
      \          \"tag\": \"ListatabFormat\",\
      \          \"contents\": {\
      \              \"ltInputSeparator\": \"\\t\",\
      \              \"ltHeaderType\": \"Comment\",\
      \              \"ltOutputSeparator\": \"\\t\"\
      \          }\
      \      }\
      \  }"





main :: IO ()
main = hspec $ do
         testSimpleConfNoListatabFormat
         testSimpleConfWithListatabFormat
         testOldConf
         testNewConf

{-# LANGUAGE OverloadedStrings #-}

module Test.BulkAPI (spec) where

import Test.Common
import Test.Import

import qualified Data.Vector as V
import qualified Lens.Micro.Aeson as LMA

newtype BulkTest =
  BulkTest Text
  deriving (Eq, Show)

instance ToJSON BulkTest where
  toJSON (BulkTest name') =
    object ["name" .= name']

instance FromJSON BulkTest where
  parseJSON = withObject "BulkTest" parse
    where
      parse o = do
        t <- o .: "name"
        BulkTest <$> parseJSON t

spec :: Spec
spec =
  describe "Bulk API" $
    it "inserts all documents we request" $ withTestEnv $ do
      _ <- insertData
      let firstTest = BulkTest "blah"
      let secondTest = BulkTest "bloo"
      let thirdTest = BulkTest "graffle"
      let fourthTest = BulkTest "garabadoo"
      let fifthTest = BulkTest "serenity"
      let firstDoc =  BulkIndex            testIndex (DocId "2") (toJSON firstTest)
      let secondDoc = BulkCreate           testIndex (DocId "3") (toJSON secondTest)
      let thirdDoc =  BulkCreateEncoding   testIndex (DocId "4") (toEncoding thirdTest)
      let fourthDoc = BulkIndexAuto        testIndex (toJSON fourthTest)
      let fifthDoc = BulkIndexEncodingAuto testIndex (toEncoding fifthTest)
      let stream = V.fromList [firstDoc, secondDoc, thirdDoc, fourthDoc, fifthDoc]
      _ <- bulk stream
      -- liftIO $ pPrint bulkResp
      _ <- refreshIndex testIndex
      -- liftIO $ pPrint refreshResp
      fDoc <- getDocument testIndex (DocId "2")
      sDoc <- getDocument testIndex (DocId "3")
      tDoc <- getDocument testIndex (DocId "4")
      -- note that we cannot query for fourthDoc and fifthDoc since we
      -- do not know their autogenerated ids.
      let maybeFirst =
            eitherDecode
            $ responseBody fDoc
              :: Either String (EsResult BulkTest)
      let maybeSecond =
            eitherDecode
            $ responseBody sDoc
            :: Either String (EsResult BulkTest)
      let maybeThird =
            eitherDecode
            $ responseBody tDoc
            :: Either String (EsResult BulkTest)
      -- liftIO $ pPrint [maybeFirst, maybeSecond, maybeThird]
      liftIO $ do
        fmap getSource maybeFirst `shouldBe` Right (Just firstTest)
        fmap getSource maybeSecond `shouldBe` Right (Just secondTest)
        fmap getSource maybeThird `shouldBe` Right (Just thirdTest)
      -- Since we can't get the docs by doc id, we check for their existence in
      -- a match all query.
      let query = MatchAllQuery Nothing
      let search = mkSearch (Just query) Nothing
      resp <- searchByIndex testIndex search
      parsed <- parseEsResponse resp :: BH IO (Either EsError (SearchResult Value))
      case parsed of
        Left e ->
          liftIO $ expectationFailure ("Expected a script-transformed result but got: " <> show e)
        (Right sr) -> do
          liftIO $
            hitsTotal (searchHits sr) `shouldBe` 6
          let nameList :: [Text]
              nameList =
                hits (searchHits sr)
                ^.. traverse
                  . to hitSource
                  . _Just
                  . LMA.key "name"
                  . _String
          liftIO $
            nameList
            `shouldBe` ["blah","bloo","graffle","garabadoo","serenity"]
{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Test.Documents where

import           Control.Monad          (void)

import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "document API" $ do
    it "indexes, updates, gets, and then deletes the generated document" $ withTestEnv $ do
      _ <- insertData
      _ <- updateData
      docInserted <- getDocument testIndex (DocId "1")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      liftIO $ fmap getSource newTweet `shouldBe` Right (Just patchedTweet)

    it "indexes, gets, and then deletes the generated document with a DocId containing a space" $ withTestEnv $ do
      _ <- insertWithSpaceInId
      docInserted <- getDocument testIndex (DocId "Hello World")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      liftIO $ fmap getSource newTweet `shouldBe` Right (Just exampleTweet)

    it "produces a parseable result when looking up a bogus document" $ withTestEnv $ do
      doc <- getDocument testIndex (DocId "bogus")
      let noTweet = eitherDecode
                    (responseBody doc) :: Either String (EsResult Tweet)
      liftIO $ fmap foundResult noTweet `shouldBe` Right Nothing

    it "can use optimistic concurrency control" $ withTestEnv $ do
      let ev = ExternalDocVersion minBound
      let cfg = defaultIndexDocumentSettings { idsVersionControl = ExternalGT ev }
      resetIndex Nothing
      res <- insertData' cfg
      liftIO $ isCreated res `shouldBe` True
      res' <- insertData' cfg
      liftIO $ isVersionConflict res' `shouldBe` True

    it "indexes two documents in a parent/child relationship and checks that the child exists" $ withTestEnv $ do
      let parentID  = "1"
          childID   = "2"
          mkParent  = DocumentParent . DocId
          parentIds = defaultIndexDocumentSettings
          childIds  = IndexDocumentSettings NoVersionControl (Just $ mkParent parentID)
          parentDoc = Parent $ (, pcLink) $ exampleTweet { message  = "Do you like haskell?" }
          childDoc pId = Child pId $ (, pcLink) $ tweetWithExtra { message  = "Yes, I do!" }

      void $ resetIndex (Just pcLink)

      -- Index parent and child
      void $ indexDocument testIndex parentIds parentDoc                      (DocId parentID)
      void $ indexDocument testIndex childIds (childDoc  $ mkParent parentID) (DocId childID)
      void $ refreshIndex testIndex
      exists <- documentExists testIndex (DocId childID)

      liftIO $ exists `shouldBe` True

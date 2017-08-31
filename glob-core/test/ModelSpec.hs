{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | The test module for model for glob
module ModelSpec
  ( spec
  ) where

import Test.Hspec
import Glob.Core.Model
import System.IO
import Internal
import Data.Time (diffUTCTime,getCurrentTime,UTCTime)
import Data.IORef
import Control.Monad
  
-- | step one: get pipe
getPipe :: IO Pipe
getPipe = connect $ readHostPort "localhost:27017"

cleanDatabase :: IO ()
cleanDatabase = void $ runDB $ dropDatabase testDBName


spec :: Spec
spec = do
  runIO $ putStrLn "Test for Model part for Glob" 
  describe "Nav and Document" $ do    
    let navItem  = Nav { navUrl   = "url"
                       , navLabel = "label"
                       , navOrder = 0
                       }
        docItem  = [ "index" =: ("label" :: String)
                   , "url"   =: ("url" :: String)
                   , "order" =: (0 :: Int)
                   ]
        docItem' = [ "labsel" =: ("label" :: String)
                   , "ural"   =: ("url" :: String)
                   , "order"  =: (0 :: Int)
                   ]  
    it "transform nav to doc" $
      nav_to_doc navItem  `shouldMatchList` docItem
    it "transform doc to nav (just)" $
      doc_to_nav docItem  `shouldBe` Just navItem
    it "transform doc to nav(nothing)" $
      doc_to_nav docItem' `shouldBe` Nothing
  describe "ResT and Document" $ do
    now <- runIO $ getCurrentTime
    let resItem = ResT { rIndex   = ["the","path","to","resource"]
                       , rRes     = Oid 233 233
                       , rType    = "type"
                       , rCTime   = now
                       , rUTime   = now
                       , rTitle   = "title"
                       , rSummary = Just "summary"
                       , rWhose   = Just "whose"
                       , rMIME    = Just "mime"
                       , rTags    = ["tag","s"]
                       }
        docItem   = [ "index"       =: ["the","path","to","resource" :: String]
                    , "res"         =: Oid 233 233
                    , "type"        =: ("type" :: String)
                    , "create-time" =: now
                    , "update-time" =: now
                    , "title"       =: ("title" :: String)
                    , "summary"     =: Just ("summary" :: String)
                    , "whose"       =: Just ("whose" :: String)
                    , "mime"        =: Just ("mime" :: String)
                    , "tags"        =: ["tag","s" :: String]
                    ]
        docItem'  = [ "inddex"      =: ["the","path","to","resource" :: String]
                    , "res"         =: Oid 233 233
                    , "type"        =: ("type" :: String)
                    , "create-time" =: now
                    , "update-time" =: now
                    , "title"       =: ("title" :: String)
                    , "summary"     =: Just ("summary" :: String)
                    , "whose"       =: Just ("whose" :: String)
                    , "mime"        =: Just ("mime" :: String)
                    , "tags"        =: ["tag","s" :: String]
                    ]
    it "transfrom res to doc" $
      res_to_doc resItem  `shouldMatchList` docItem
    it "transform doc to res(just)" $
      doc_to_res docItem  `shouldBe` Just resItem
    it "transform doc to res(nothing)" $
      doc_to_res docItem' `shouldBe` Nothing
  describe "database actions" $ do
    describe "for context" $ beforeAll_ cleanDatabase $ do
      let contextTestCollection = "ctc"
          contextTestField      = "ctf"
          contextTestItem       = "cti"    :: String
          contextTestItemNew    = "ctinew" :: String
          contextTestOidBad (Oid a b) = Oid (a-1) (b-1) 
      contextTestOid <- runIO $ newIORef Nothing
      it "update a context(new)" $ do
        cto <- readIORef contextTestOid
        oid <- runDB $ update_context
          contextTestCollection
          cto
          contextTestField
          contextTestItem
        Just doc <- runDB $ findOne (select ["_id" =: oid] contextTestCollection)
        writeIORef contextTestOid $ Just oid
        doc `shouldContain` [ "_id"            =: oid
                            , contextTestField =: contextTestItem
                            ]
      it "update context(again)" $ do
        cto <- readIORef contextTestOid
        oid <- runDB $ update_context
          contextTestCollection
          cto
          contextTestField
          contextTestItemNew
        Just doc <- runDB $ findOne (select ["_id" =: oid] contextTestCollection)
        writeIORef contextTestOid $ Just oid
        doc `shouldContain` [ "_id"            =: oid
                            , contextTestField =: contextTestItemNew
                            ]
      it "fetch context(just)" $ do        
        Just cto <- readIORef contextTestOid
        rt <- runDB $ fetch_context
          contextTestField
          ResT {rRes = cto}
          contextTestCollection
        rt `shouldBe` Just contextTestItemNew
      it "fetch context(nothing)" $ do        
        Just cto <- readIORef contextTestOid
        rt <- runDB $ fetch_context
          contextTestField
          ResT {rRes = contextTestOidBad cto}
          contextTestCollection
        let shouldBE :: Maybe String -> Maybe String -> IO ()
            shouldBE = shouldBe
        rt `shouldBE` Nothing
      it "delete context" $ do
        Just cto <- readIORef contextTestOid
        runDB $ delete_context ResT{rRes = cto} contextTestCollection
        rt <- runDB $ rest =<< find (select [] contextTestCollection)
        rt `shouldBe` []
    describe "for res" $ beforeAll_ cleanDatabase $ do
      now <- runIO $ getCurrentTime
      let resItem1 = ResT { rIndex   = ["the","path","to","resource","1"]
                          , rRes     = Oid 233 233
                          , rType    = "type"
                          , rCTime   = now
                          , rUTime   = now
                          , rTitle   = "title"
                          , rSummary = Just "summary"
                          , rWhose   = Just "whose"
                          , rMIME    = Just "mime"
                          , rTags    = ["tag","s"]
                          }
          resItem2 = ResT { rIndex   = ["the","path","to","resource","2"]
                          , rRes     = Oid 233 233
                          , rType    = "type"
                          , rCTime   = now
                          , rUTime   = now
                          , rTitle   = "title"
                          , rSummary = Just "summary"
                          , rWhose   = Just "whose"
                          , rMIME    = Just "mime"
                          , rTags    = ["tag","s"]
                          }                     
          docItem1  = [ "index"       =: ["the","path","to","resource","1" :: String]
                      , "res"         =: Oid 233 233
                      , "type"        =: ("type" :: String)
                      , "create-time" =: now
                      , "update-time" =: now
                      , "title"       =: ("title" :: String)
                      , "summary"     =: Just ("summary" :: String)
                      , "whose"       =: Just ("whose" :: String)
                      , "mime"        =: Just ("mime" :: String)
                      , "tags"        =: ["tag","s" :: String]
                      ]                   
          docItem2  = [ "index"       =: ["the","path","to","resource","2" :: String]
                      , "res"         =: Oid 233 233
                      , "type"        =: ("type" :: String)
                      , "create-time" =: now
                      , "update-time" =: now
                      , "title"       =: ("title" :: String)
                      , "summary"     =: Just ("summary" :: String)
                      , "whose"       =: Just ("whose" :: String)
                      , "mime"        =: Just ("mime" :: String)
                      , "tags"        =: ["tag","s" :: String]
                      ]
      it "update a new resource index" $ do
        runDB $ update_res resItem1
        rt:_ <- runDB $ rest =<< find (select [] "index")
        doc_to_res rt `shouldBe` Just resItem1
      it "fetch a resource index" $ do
        rt1 <- runDB $ fetch_res ["the","path","to","resource","1"]
        rt2  <- runDB $ fetch_res ["the","path","to","resource","2"]
        rt1 `shouldBe` Just resItem1
        rt2 `shouldBe` Nothing
      it "fetch all inedxes, when there are indexes in database" $ do
        runDB $ update_res resItem2
        rt <- runDB $ fetch_res_all
        rt `shouldBe` [resItem1,resItem2]
      it "delete a index" $ do
        runDB $ delete_res resItem1
        rt1:_ <- runDB $ rest =<< find (select [] "index")
        doc_to_res rt1 `shouldBe` Just resItem2
        runDB $ delete_res resItem2
        rt2 <- runDB $ rest =<< find (select [] "index")
        rt2 `shouldBe` []
      it "fetch all indexes, when database is empty" $ do
        rt <- runDB $ fetch_res_all
        rt `shouldBe` []
        
        
     

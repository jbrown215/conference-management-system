module Handler.ReviewPaperSpec (spec) where

import TestImport
import TestTools
import Data.PaperStatus
import qualified Data.Text as T

spec :: Spec
spec = withApp $ do
    it "GET: requires authorization to access /review" $ do
        failsAuth GET ("/review/1" :: Text)

    it "GET: pc does not have accces to /review/0 if not assigned paper 0" $ do
        Entity uid user <- createUser "Jordan" "example@test.com" "pwd" True True 
        _ <- runDB $ insert $ Paper uid "" "" "" ("" :: ByteString) True True
        authenticateAs $ Entity uid user
        failsAuth404 GET ("/review/1" :: Text)

    it "GET: reviewer has access to a paper they should review" $ do
        Entity uid user <- createUser "Jordan" "example@test.com" "pwd" True False 
        pid <- runDB $ insert $ Paper uid "" "" "" ("" :: ByteString) True False 
        rid <- runDB $ insert $ Review pid uid SReject ""
        authenticateAs $ Entity uid user
        putStrLn (("/review/" :: Text) ++ (T.pack (show rid)))
        get (ReviewPaperR rid)
        statusIs 200

    it "POST: requires authorization to access /review" $ do
        failsAuth POST ("/review/1" :: Text)

    it "POST: pc does not have accces to /review/0 if not assigned paper 0" $ do
        Entity uid user <- createUser "Jordan" "example@test.com" "pwd" True True 
        _ <- runDB $ insert $ Paper uid "" "" "" ("" :: ByteString) True True
        authenticateAs $ Entity uid user
        failsAuth404 POST ("/review/1" :: Text)

    it "POST: reviewer has access to a paper they should review" $ do
        Entity uid user <- createUser "Jordan" "example@test.com" "pwd" True False 
        pid <- runDB $ insert $ Paper uid "" "" "" ("" :: ByteString) True False 
        rid <- runDB $ insert $ Review pid uid SReject ""
        authenticateAs $ Entity uid user
        putStrLn (("/review/" :: Text) ++ (T.pack (show rid)))
        post (ReviewPaperR rid)
        statusIs 303

module Handler.ViewReviewSpec (spec) where

import TestImport
import TestTools
import qualified Data.ConferencePhase as C
import Data.PaperStatus

spec :: Spec
spec = withApp $ do
    it "requires authorization to access /see-reviews" $ do
        Entity uid _user <- createUser "Jordan" "example@test.com" "pwd" True True 
        pid <- runDB $ insert $ Paper uid "" "" "" ("" :: ByteString) True False 
        _ <- runDB $ insert $ Review pid uid SReject ""
        failsAuth GET ("/see-reviews/0" :: Text)

    it "pc has accces to /see-reviews" $ do
        Entity uid _user <- createUser "Jordan" "example@test.com" "pwd" False False 
        user2 <- createUser "Jordan2" "example2@test.com" "pwd" True True 
        pid <- runDB $ insert $ Paper uid "" "" "" ("" :: ByteString) True False 
        _ <- runDB $ insert $ Review pid uid SReject ""
        authenticateAs user2
        get $ ViewReviewR pid
        statusIs 200

    it "authors dont have access to /see-reviews until decision phase" $ do
        Entity uid user <- createUser "Jordan" "example@test.com" "pwd" False False 
        pid <- runDB $ insert $ Paper uid "" "" "" ("" :: ByteString) True False 
        _ <- runDB $ insert $ Review pid uid SReject ""
        _ <- runDB $ insert $ Author "Jordan" uid pid
        authenticateAs $ Entity uid user
        get $ ViewReviewR pid
        statusIs 403
        runDB $ deleteWhere ([] :: [Filter CurrentPhase])
        _ <- runDB $ insert $ CurrentPhase C.Decision
        get $ ViewReviewR pid
        statusIs 200 

module Handler.ReadySpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    it "requires authorization to access /ready" $ do
        failsAuth POST ("/ready/0" :: Text)

    it "pc does not have accces to /ready for a paper they didn't write" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True True 
        authenticateAs user
        failsAuth POST ("/ready/0" :: Text)

    it "paper owner can access ready" $ do
        Entity uid user <- createUser "Jordan" "example@test.com" "pwd" True True 
        _ <- runDB $ insert $ Paper uid "" "" "" ("" :: ByteString) True True
        authenticateAs $ Entity uid user
        failsAuth POST ("/ready/0" :: Text)

    it "paper owner can access ready" $ do
        Entity uid _user <- createUser "Jordan" "example@test.com" "pwd" True True 
        Entity uid2 user2 <- createUser "Jordan2" "example2@test.com" "pwd" True True 
        pid <- runDB $ insert $ Paper uid "" "" "" ("" :: ByteString) True True
        _ <- runDB $ insert $ Author "" uid2 pid
        authenticateAs $ Entity uid2 user2
        failsAuth POST ("/ready/0" :: Text)

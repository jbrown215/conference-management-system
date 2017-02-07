module Handler.ReviewSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    it "requires authorization to access /review" $ do
        failsAuth GET ("/review" :: Text)

    it "reviewer gets access to /review" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True True 
        authenticateAs $ user
        passesAuth GET ("/review" :: Text)

    it "non-reviewer doesn't get access to /review" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" False True 
        authenticateAs $ user
        failsAuth GET ("/review" :: Text)

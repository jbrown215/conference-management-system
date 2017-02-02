module Handler.ProgramChairSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    it "requires authorization to access /pc" $ do
        failsAuth GET ("/pc" :: Text)

    it "pc has accces to /pc" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True True 
        authenticateAs user
        passesAuth GET ("/pc" :: Text)

    it "reviewer who is not pc does not have access to /pc" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True False
        authenticateAs user
        failsAuth GET ("/pc" :: Text)

    it "a regular user cannot access /pc" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" False False
        authenticateAs user
        failsAuth GET ("/pc" :: Text)

module Handler.SetPhaseSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    it "requires authorization to access /set-phase" $ do
        failsAuth POST ("/set-phase" :: Text)

    it "pc has accces to /set-phase" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True True 
        authenticateAs user
        passesAuth POST ("/set-phase" :: Text)

    it "reviewer who is not pc does not have access to /set-phase" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True False
        authenticateAs user
        failsAuth POST ("/set-phase" :: Text)

    it "a regular user cannot access /set-phase" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" False False
        authenticateAs user
        failsAuth POST ("/set-phase" :: Text)

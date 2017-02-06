module Handler.FinalDecisionSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    it "requires authorization to access /final-decision" $ do
        failsAuth POST ("/final-decision/0/True" :: Text)

    it "pc has accces to /assign" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True True 
        authenticateAs user
        passesAuth POST ("/final-decision/0/True" :: Text)

    it "reviewer who is not pc does not have access to /final-decision" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True False
        authenticateAs user
        failsAuth POST ("/final-decision/0/True" :: Text)

    it "a regular user cannot access /final-decision" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" False False
        authenticateAs user
        failsAuth POST ("/final-decision/0/True" :: Text)

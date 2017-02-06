module Handler.AssignPaperSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    it "requires authorization to access /assign" $ do
        failsAuth POST ("/assign" :: Text)

    it "pc has accces to /assign" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True True 
        authenticateAs user
        passesAuth POST ("/assign" :: Text)

    it "reviewer who is not pc does not have access to /assign" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True False
        authenticateAs user
        failsAuth POST ("/assign" :: Text)

    it "a regular user cannot access /assign" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" False False
        authenticateAs user
        failsAuth404 POST ("/assgin" :: Text)

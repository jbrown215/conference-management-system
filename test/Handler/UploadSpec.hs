module Handler.UploadSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    it "GET: requires authorization to access /upload" $ do
        failsAuth GET ("/upload" :: Text)

    it "GET: any logged in user can access /upload" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" False False
        authenticateAs user
        passesAuth GET ("/upload" :: Text)

    it "POST: requires authorization to access /upload" $ do
        failsAuth POST ("/upload" :: Text)

    it "POST: any logged in user can access /upload" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" False False
        authenticateAs user
        passesAuth POST ("/upload" :: Text)

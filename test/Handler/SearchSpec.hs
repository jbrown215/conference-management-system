module Handler.SearchSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    it "requires authorization to access /search" $ do
        failsAuth GET ("/search/test" :: Text)

    it "authenticated user gets access to /search" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True True 
        authenticateAs user
        passesAuth GET ("/search/test" :: Text)

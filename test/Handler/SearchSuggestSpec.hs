module Handler.SearchSuggestSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    it "requires authorization to access /search-suggest" $ do
        failsAuth GET ("/search-suggest" :: Text)

    it "authenticated user gets access to /search-suggest" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" True True 
        authenticateAs user
        passesAuth GET ("/search-suggest" :: Text)

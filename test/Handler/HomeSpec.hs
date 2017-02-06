module Handler.HomeSpec (spec) where

import TestImport
import TestTools

spec :: Spec
spec = withApp $ do
    it "requires authorization to access /" $ do
        failsAuthRedirect GET ("/" :: Text)

    it "any logged in user can access /" $ do
        user <- createUser "Jordan" "example@test.com" "pwd" False False
        authenticateAs user
        passesAuth GET ("/" :: Text)

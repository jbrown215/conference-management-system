module TestTools (
    failsAuth,
    passesAuth,
    StdMethod(..)
) where

import TestImport
import Yesod.Core         (RedirectUrl)
import Network.HTTP.Types (StdMethod(..), renderStdMethod)

passesAuth :: RedirectUrl App url =>
                 StdMethod -> url -> YesodExample App ()
passesAuth method url = do
    request $ do
        setMethod $ renderStdMethod method
        setUrl url
    statusIs 200

failsAuth :: RedirectUrl App url =>
                 StdMethod -> url -> YesodExample App ()
failsAuth method url = do
    request $ do
        setMethod $ renderStdMethod method
        setUrl url
    statusIs 403

module TestTools (
    failsAuth,
    failsAuth404,
    failsAuthRedirect,
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
    statusIs (if method == POST then 303 else 200)

failsAuth :: RedirectUrl App url =>
                 StdMethod -> url -> YesodExample App ()
failsAuth method url = do
    request $ do
        setMethod $ renderStdMethod method
        setUrl url
    statusIs 403

failsAuthRedirect :: RedirectUrl App url =>
                 StdMethod -> url -> YesodExample App ()
failsAuthRedirect method url = do
    request $ do
        setMethod $ renderStdMethod method
        setUrl url
    statusIs 303

failsAuth404 :: RedirectUrl App url =>
                 StdMethod -> url -> YesodExample App ()
failsAuth404 method url = do
    request $ do
        setMethod $ renderStdMethod method
        setUrl url
    statusIs 404

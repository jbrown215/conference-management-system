{-# Language TemplateHaskell #-}

module Handler.Review where

import Import
import DB
import qualified Database.Esqueleto as E

{-@ invariant {v:AuthResult | isAuthorized v} @-}
getReviewR :: Handler Html
getReviewR = do
    papers <- getPapersToReview
    (uid, user) <- requireAuthPair
    allPapers <- getAllViewablePapers uid
    defaultLayout $ do
        $(widgetFile "review")

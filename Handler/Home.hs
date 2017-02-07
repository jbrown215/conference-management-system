{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module Handler.Home where

import Import
import DB
import Data.ConferencePhase

getHomeR :: Handler Html
getHomeR = do
    (_uid, user) <- requireAuthPair
    Entity _pid phase <- getCurrentPhase
    let isDecisionPhase = if currentPhasePhase phase == Decision then True else False
    papers <- getPapers
    authorLists <- mapM getAuthorListForPaper papers
    acceptedPapers <- getAcceptedPapers
    acceptedAuthorLists <- mapM getAuthorListForPaper acceptedPapers
    let authorsAndPapers = Import.zip authorLists papers
    let acceptedAuthorsAndPapers = Import.zip acceptedAuthorLists acceptedPapers
    defaultLayout $ do
        setTitle Import.. toHtml $ userUsername user <> "'s User page"
        $(widgetFile "homepage")

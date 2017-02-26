{-# Language OverloadedStrings #-}
{-# Language FlexibleInstances #-}
{-# Language TemplateHaskell #-}

module Handler.Home where

import Import
import DB
import Data.ConferencePhase

{-@ type Handler a = {h:HandlerT App IO a | true}@-}

{-@ data Tagged a <p :: User->Bool> = Tagged (content :: a) @-}
data Tagged a = Tagged a

instance ToContent (Tagged Html) where
    toContent (Tagged x) = toContent x

instance ToTypedContent (Tagged Html) where
    toTypedContent (Tagged a) = toTypedContent a

{-@ measure isDecision @-}
isDecision :: ConferencePhase -> Bool
isDecision Decision = True
isDecision _ = False

{-@ data User = User { userUsername :: Text
                , userPassword :: ByteString
                , userEmailAddress :: Text
                , userVerified :: Bool
                , userVerifyKey :: Text
                , userResetPasswordKey :: Text
                , userReviewer :: Bool
                , userPc :: Bool
                }
@-}

{-@ type Pc = {u:User | userPc u} @-}

{-@ filterUsers :: [User] -> [Pc] @-}
filterUsers :: [User] -> [User]
filterUsers [] = []
filterUsers (x:xs) = if userPc x then x : (filterUsers xs) else filterUsers xs

{-@ getPcUsers :: Handler [Pc] @-}
getPcUsers :: Handler [User]
getPcUsers = do 
    users <- runDB $ selectList [UserPc ==. True] []
    let usersMapped = Import.map (\(Entity _ x) -> x) users
    let usersFiltered = filterUsers usersMapped
    return usersFiltered

{-@ data Paper = Paper {
                   paperOwner :: UserId
                 , paperFilepath :: Text
                 , paperTitle :: Text
                 , paperAbstract :: Text
                 , paperContent :: ByteString
                 , paperReady :: Bool
                 , paperPcAccepted :: Bool
                 }
@-}

{-@ filterPapers :: u:UserId -> [Paper] -> [{p:Paper | paperOwner p == u}] @-}
filterPapers :: UserId -> [Paper] -> [Paper]
filterPapers _u [] = []
filterPapers u (x:xs) = if paperOwner x == u then x : (filterPapers u xs) else filterPapers u xs

{-@ getPapersForUser :: u:UserId -> Handler [{p:Paper | paperOwner p == u}] @-}
getPapersForUser :: UserId -> Handler [Paper]
getPapersForUser u = do
    papers <- runDB $ selectList [PaperOwner ==. u] []
    let papersMapped = Import.map (\(Entity _ x) -> x) papers
    let papersFiltered = filterPapers u papersMapped
    return papersFiltered

{-@ checkPc :: Pc -> Bool @-}
checkPc :: User -> Bool
checkPc u = userPc u

{-@ getHomeR :: Handler (Tagged Html) @-}
getHomeR :: Handler (Tagged Html)
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
    let _ = if userPc user then checkPc user else False
    pls <- defaultLayout $ do
        setTitle Import.. toHtml $ userUsername user <> "'s User page"
        $(widgetFile "homepage")
    return (Tagged pls)

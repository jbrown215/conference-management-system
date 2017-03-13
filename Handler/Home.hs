{-# Language OverloadedStrings #-}
{-# Language FlexibleInstances #-}
{-# Language TemplateHaskell #-}
{-# Language EmptyDataDecls #-}

module Handler.Home where

import Import
import DB
import Data.ConferencePhase

{-@ type Handler a = HandlerT App IO a @-}

{-@ data variance HandlerT covariant covariant covariant contravariant @-}

{-@ data Tagged a <p :: User->Bool> = Tagged (content :: a) @-}
data Tagged a = Tagged a
data World

instance ToContent (Tagged Html) where
    toContent (Tagged x) = toContent x

instance ToTypedContent (Tagged Html) where
    toTypedContent (Tagged a) = toTypedContent a

instance Functor Tagged where
  fmap f (Tagged x) = Tagged (f x)

instance Applicative Tagged where
  pure  = Tagged
  -- f (a -> b) -> f a -> f b 
  (Tagged f) <*> (Tagged x) = Tagged (f x)

instance Monad Tagged where
  return x = Tagged x
  (Tagged x) >>= f = f x 
  (Tagged _) >>  t = t  
  fail          = error

{-@ instance Monad Tagged where 
     >>= :: forall <p :: User -> Bool, f:: a -> b -> Bool>. 
            x:Tagged <p> a
         -> (u:a -> Tagged <p> (b <f u>))
         -> Tagged <p> (b<f (content x)>); 
     >>  :: x:Tagged a
         -> Tagged b
         -> Tagged b;
     return :: forall <p :: User -> Bool>. a -> Tagged <p> a 
  @-}

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


{-@ measure author :: UserId -> Paper -> Bool @-}
{-@ assume isAuthor :: u:UserId -> p:Paper -> {v:Bool | v == author u p} @-}
-- This seems to be a way to show relationships that can't be put into type signatures.
-- It feels REALLY unsafe to have to assume that this is correct. It would be nice if
-- We could find a sane and sound way to generate these functions. They happen when there is a 
-- model in the DB that represents a relationship between other tables. Really, this
-- function should only return true if there is some Author object where its author Id
-- and its Paper id are equal to the input UserId and Paper. This will also be very
-- expensive to do, since we will have to make a DB call for each paper.
-- refinement reflection:
-- provide a proof that selectList would give us true iff isAuthor holds.
isAuthor :: UserId -> Paper -> Bool
isAuthor id p = True

{-@ filterPapers :: u:UserId -> [Paper] -> [{p:Paper | author u p}] @-}
filterPapers :: UserId -> [Paper] -> [Paper]
filterPapers _t [] = []
filterPapers u (x:xs) = if (isAuthor u x) then x : (filterPapers u xs) else filterPapers u xs

{-@ getPapersForUser :: u:UserId -> Handler [{p:Paper | author u p}] @-}
getPapersForUser :: UserId -> HandlerT App IO [Paper]
getPapersForUser u = do
    papers <- runDB $ selectList [PaperOwner ==. u] []
    let papersMapped = Import.map (\(Entity _ x) -> x) papers
    let papersFiltered = filterPapers u papersMapped
    return papersFiltered


{-@ checkPc :: User -> HandlerT <{\u -> true}> App IO Bool @-}
checkPc :: User -> Handler Bool
checkPc u = return $ userPc u

{-@ getHomeR :: forall <p :: User -> Bool>. HandlerT <{\u -> true}> App IO (Tagged Html) @-}
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
    _ <- checkPc user
    pls <- defaultLayout $ do
        setTitle Import.. toHtml $ userUsername user <> "'s User page"
        $(widgetFile "homepage")
    return (Tagged pls)

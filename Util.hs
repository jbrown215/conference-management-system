{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import Import
import DB

reviewerOpts :: Handler [(Text, Key User)]
reviewerOpts = do
    reviewerEnts <- getReviewers
    return $ map (\(Entity uid user) -> (userUsername user, uid)) reviewerEnts

allUserOpts :: Handler [(Text, Key User)]
allUserOpts = do
    users :: [Entity User] <- runDB $ selectList [] []
    return $ map (\(Entity uid user) -> (userUsername user ++ " (" ++ (userEmailAddress user) ++ ")" , uid)) users

paperOpts :: Handler [(Text, Key Paper)]
paperOpts = do
    paperEnts <- getAllPapers
    return $ map (\(Entity pid paper) -> (paperTitle paper, pid)) paperEnts 

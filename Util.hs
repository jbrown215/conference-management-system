module Util where

import Import
import DB

reviewerOpts :: Handler [(Text, Key User)]
reviewerOpts = do
    reviewerEnts <- getReviewers
    return $ Import.map (\(Entity uid user) -> (userUsername user, uid)) reviewerEnts

paperOpts :: Handler [(Text, Key Paper)]
paperOpts = do
    paperEnts <- getAllPapers
    return $ Import.map (\(Entity pid paper) -> (paperTitle paper, pid)) paperEnts 

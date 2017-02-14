module Handler.RemoveReviewer where

import Import
import qualified Handler.ProgramChair as PC
import qualified Util as U

postRemoveReviewerR :: Handler Html
postRemoveReviewerR = do
    reviewerOpts <- U.reviewerOpts
    ((removeResult, _), _) <- runFormPost $ PC.removeReviewerForm reviewerOpts
    case removeResult of
        FormSuccess (PC.RemoveReviewerForm userId) -> do
            runDB $ update userId [UserReviewer =. False]
            setMessage "Reviewer Removed"
            redirect ProgramChairR
        _ -> do
            setMessage "Something went wrong"
            redirect $ ProgramChairR

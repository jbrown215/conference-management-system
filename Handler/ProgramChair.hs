module Handler.ProgramChair where

import Import
import DB
import Data.ConferencePhase as C
import qualified Util as U
import qualified Database.Esqueleto as E
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data AddReviewerForm = AddReviewerForm 
    { userId :: Key User
    }

data RemoveReviewerForm = RemoveReviewerForm 
    { removeId :: Key User
    }

data AssignPaperForm = AssignPaperForm
    { user  :: Key User
    , paper :: Key Paper
    }

data SetPhaseForm = SetPhaseForm
    { phase :: ConferencePhase }

getProgramChairR :: Handler Html
getProgramChairR = do 
    reviewers <- getReviewers
    reviewerOpts <- U.reviewerOpts
    userOpts <- U.allUserOpts
    papersForReviewers <- mapM getPapersForReviewer reviewers
    Entity _currentPhaseId currentPhase <- getCurrentPhase
    let reviewersAndPapers = zip reviewers papersForReviewers
    (formWidget, formEnctype) <- generateFormPost $ addReviewerForm userOpts 
    paperOpts <- U.paperOpts
    (assignFormWidget, assignFormEnctype) <-
        generateFormPost $ assignPaperForm reviewerOpts paperOpts 
    (removeFormWidget, removeFormEnctype) <-
        generateFormPost $ removeReviewerForm reviewerOpts
    (setPhaseFormWidget, setPhaseFormEnctype) <- generateFormPost $ setPhaseForm
    defaultLayout $ do
        $(widgetFile "pc")

postProgramChairR :: Handler Html
postProgramChairR = do
    userOpts <- U.allUserOpts
    ((addResult, _), _) <- runFormPost $ addReviewerForm userOpts
    case addResult of
        FormSuccess (AddReviewerForm uid) -> do
            runDB $ update uid [UserReviewer =. True]
            setMessage "Review Saved"
            redirect ProgramChairR
        _ -> do
            setMessage "Something went wrong"
            redirect $ ProgramChairR
    

addReviewerForm :: [(Text, Key User)] -> Form AddReviewerForm
addReviewerForm users = renderBootstrap3 BootstrapBasicForm $ AddReviewerForm 
    <$> areq (selectFieldList users) "Reviewer" Nothing 

removeReviewerForm :: [(Text, Key User)] -> Form RemoveReviewerForm 
removeReviewerForm reviewers = renderBootstrap3 BootstrapBasicForm $ RemoveReviewerForm 
    <$> areq (selectFieldList reviewers) "Reviewer" Nothing 

assignPaperForm :: [(Text, Key User)] -> [(Text, Key Paper)] -> Form AssignPaperForm
assignPaperForm reviewers papers = renderBootstrap3 BootstrapBasicForm $ AssignPaperForm 
    <$> areq (selectFieldList reviewers) "Reviewer" Nothing 
    <*> areq (selectFieldList papers) "Paper" Nothing

setPhaseForm :: Form SetPhaseForm
setPhaseForm = renderBootstrap3 BootstrapBasicForm $ SetPhaseForm 
    <$> areq (selectFieldList phases) "Set phase" Nothing 
  where
    phases :: [(Text, ConferencePhase)]
    phases = [("Submission", C.Submission), ("Review", C.Review), ("Decision", C.Decision)]

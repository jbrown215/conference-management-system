module Handler.DeletePaper where

import Import

postDeletePaperR :: PaperId -> Handler Html
postDeletePaperR paperId = do
   runDB $ deleteWhere [AuthorPaper ==. paperId]
   runDB $ deleteWhere [ReviewPaper ==. paperId]
   runDB $ deleteWhere [ConflictPaper ==. paperId]
   runDB $ delete paperId 
   setMessage "Deleted Paper"
   redirect HomeR

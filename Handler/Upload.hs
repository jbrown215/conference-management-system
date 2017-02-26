module Handler.Upload where

import Import
import DB
import qualified Util as Util
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit.Binary
import qualified Data.ConferencePhase as C

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), BootstrapGridOptions (..), renderBootstrap3, bfs)
import Yesod.Form.Jquery (jqueryAutocompleteField)

data FileForm = FileForm
    { fileInfo :: FileInfo
    , title    :: Text
    , author1  :: Text
    , author2  :: Maybe Text
    , author3  :: Maybe Text
    , author4  :: Maybe Text
    , abstract :: Textarea
    , conflicts :: Maybe [Key User]
    }

aToList :: Maybe Text -> [Text]
aToList (Just a) = [a]
aToList Nothing = []

postUploadR :: Handler Html
postUploadR = do
    reviewerOpts <- Util.reviewerOpts
    (uid, user) <- requireAuthPair
    ((result, _), _) <- runFormPost $ uploadForm (userEmailAddress user) reviewerOpts
    Entity _pid phase <- getCurrentPhase
    case currentPhasePhase phase of
        C.Submission ->
            case result of
                FormSuccess (FileForm fi title a1 a2 a3 a4 abstract mConflicts) -> do
                    -- I'm sad that I had to do this, but making a custom field was hard!
                    let authors = [a1] ++ (aToList a2) ++ (aToList a3) ++ (aToList a4)
                    case not (fileContentType fi == "application/pdf") of
                        True -> do
                            setMessage "File must be a PDF"
                            redirect UploadR
                        False -> do
                            fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
                            -- We get the author ids first so that we can fail if some email was incorrect before adding the paper
                            authorIds  <- mapM (getUserForUsername UploadR) authors
                            let authorsAndIds = zip authors authorIds
                            paperId <- runDB $ insert $ Paper uid (fileName fi)
                                    title (unTextarea abstract)
                                        (S.pack . L.unpack $ fileBytes) False False
                            _ <- runDB $ mapM (\(author, Entity aUid _a) -> insert_ $ Author author aUid paperId) authorsAndIds
                            case mConflicts of
                                Just conflicts -> do
                                    _ <- runDB $ mapM (\conflict -> insert_ $ Conflict paperId conflict) conflicts
                                    return ()
                                _ -> do
                                return ()
                            setMessage "PDF saved"
                            redirect HomeR
                _ -> do
                    setMessage "Something went wrong"
                    redirect UploadR 
        _ -> do
            setMessage "Sorry, we are no longer accepting submissions."
            redirect UploadR 

getUploadR :: Handler Html
getUploadR = do
    reviewerOpts <- Util.reviewerOpts
    (_uid, user) <- requireAuthPair
    (formWidget, formEnctype) <- generateFormPost $ uploadForm (userEmailAddress user) reviewerOpts
    defaultLayout $ do
        $(widgetFile "upload")

uploadForm :: Text -> [(Text, Key User)] -> Form FileForm
uploadForm email reviewerOpts = renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColLg 2) (ColSm 0) (ColLg 10)) $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField (bfs ("Paper Title" :: Text)) Nothing
    <*> areq (jqueryAutocompleteField SearchSuggestR) (bfs ("Author Email 1" :: Text)) (Just email) 
    <*> aopt (jqueryAutocompleteField SearchSuggestR) (bfs ("Author Email 2" :: Text)) Nothing
    <*> aopt (jqueryAutocompleteField SearchSuggestR) (bfs ("Author Email 3" :: Text)) Nothing
    <*> aopt (jqueryAutocompleteField SearchSuggestR) (bfs ("Author Email 4" :: Text)) Nothing
    <*> areq textareaField (bfs ("Abstract" :: Text)) Nothing
    <*> aopt (checkboxesFieldList reviewerOpts) "Conflicts" Nothing

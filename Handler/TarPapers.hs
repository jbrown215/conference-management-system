{-# LANGUAGE ScopedTypeVariables #-}

module Handler.TarPapers where

import Import
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Codec.Archive.Tar as Tar

getTarPapersR :: Handler ()
getTarPapersR = do
    let filename = "all-papers.tar"
    _ <- writeAllPapers
    _ <- liftIO $ Tar.create filename "." ["papers"]
    bytes <- liftIO $ B.readFile filename
    addHeader "Content-Disposition" $ T.concat
        [ "attachment; filename=\"" ++ filename ++ "\""]
    sendResponse (T.encodeUtf8 $ "application/x-tar", toContent bytes)

writeAllPapers :: Handler () 
writeAllPapers = do
    papers :: [Entity Paper] <- runDB $ selectList [] []
    let titlesAndBytes = map (\(Entity pid paper) -> ((T.unpack $ paperName (Entity pid paper)), paperContent paper)) papers
    _ <- liftIO $ traverse (\(path, bytes) -> B.writeFile path bytes) titlesAndBytes 
    return ()
  where 
    paperName :: Entity Paper -> Text
    paperName (Entity pid paper) = take 50 $
        "papers/" 
        ++ (toPathPiece pid)
        ++ "-"
        ++ (paperTitle paper)
        ++ "-"
        ++ (paperFilepath paper)

module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
#if MIN_VERSION_classy_prelude(1, 0, 0)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
#else
import ClassyPrelude         as X hiding (delete, deleteBy)
#endif
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X

-- Wiping the database
import Database.Persist.Sqlite              (sqlDatabase, wrapConnection, createSqlPool)
import qualified Database.Sqlite as Sqlite
import Control.Monad.Logger                 (runLoggingT)
import Settings (appDatabaseConf)
import Yesod.Core (messageLoggerSource)
import Yesod.Auth.Account
import Data.ConferencePhase

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation True settings 
    wipeDB foundation
    sqliteConn <- rawConnection (sqlDatabase $ appDatabaseConf settings)
    disableForeignKeys sqliteConn
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    pool <- runLoggingT (createSqlPool (wrapConnection sqliteConn) 1) logFunc
    _ <- liftIO $ runSqlPersistMPool (insert $ CurrentPhase Submission) pool
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

setUpDB :: YesodExample App (Key CurrentPhase)
setUpDB = runDB $ insert $ CurrentPhase Submission

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    -- In order to wipe the database, we need to temporarily disable foreign key checks.
    -- Unfortunately, disabling FK checks in a transaction is a noop in SQLite.
    -- Normal Persistent functions will wrap your SQL in a transaction,
    -- so we create a raw SQLite connection to disable foreign keys.
    -- Foreign key checks are per-connection, so this won't effect queries outside this function.

    -- Aside: SQLite by default *does not enable foreign key checks*
    -- (disabling foreign keys is only necessary for those who specifically enable them).
    let settings = appSettings app
    sqliteConn <- rawConnection (sqlDatabase $ appDatabaseConf settings)
    disableForeignKeys sqliteConn

    let logFunc = messageLoggerSource app (appLogger app)
    pool <- runLoggingT (createSqlPool (wrapConnection sqliteConn) 1) logFunc

    flip runSqlPersistMPool pool $ do
        tables <- getTables
        sqlBackend <- ask
        let queries = map (\t -> "DELETE FROM " ++ (connEscapeName sqlBackend $ DBName t)) tables
        forM_ queries (\q -> rawExecute q [])
    return ()

rawConnection :: Text -> IO Sqlite.Connection
rawConnection t = Sqlite.open t

disableForeignKeys :: Sqlite.Connection -> IO ()
disableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = OFF;" >>= void . Sqlite.step

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- return ()
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userEmailAddress u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.
createUser :: Text -> Text -> Text -> Bool -> Bool -> YesodExample App (Entity User)
createUser name email pwd reviewer pc = do
    key <- newVerifyKey
    hashed <- hashPassword pwd
    runDB $ insertEntity User
        { userUsername = name
        , userPassword = hashed
        , userEmailAddress = email
        , userVerified = True
        , userVerifyKey = ""
        , userResetPasswordKey = ""
        , userReviewer = reviewer
        , userPc = pc
        }

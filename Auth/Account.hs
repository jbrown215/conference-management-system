module Auth.Account where

import Import.NoFoundation
import Yesod.Auth.Account
import qualified Yesod.Auth.Message as Msg
import qualified Data.Text as T
import Control.Monad.Trans.RWS.Lazy
import Text.Blaze.Internal

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), BootstrapGridOptions (..), renderBootstrap3, bfs)

getNewAccountR :: YesodAuthAccount db site 
                  => HandlerT Auth (HandlerT site IO) Html
getNewAccountR = do
    tm <- getRouteToParent
    lift $ defaultLayout $ do
        setTitleI Msg.RegisterLong
        customNewAccountWidget tm 

postNewAccountR :: YesodAuthAccount db site 
                   => HandlerT Auth (HandlerT site IO) Html
postNewAccountR = do
    tm <- getRouteToParent
    mr <- lift getMessageRender
    ((result, _), _) <- lift $ runFormPost $ customNewAccountForm 
    mdata <- case result of
                FormMissing -> invalidArgs ["Form is missing"]
                FormFailure msg -> return $ Left msg
                FormSuccess d -> return $ if password1 d == password2 d
                                    then Right d
                                    else Left [mr Msg.PassMismatch]
    case mdata of
        Left errs -> do
            setMessage $ toHtml $ T.concat errs
            redirect newAccountR

        Right d -> do route <- lift $ createNewCustomAccount d tm
                      redirect route

-- | The data collected in the new account form.
data CustomNewAccountData = CustomNewAccountData {
      newAccountEmail :: Text
    , newAccountName :: Text
    , password1 :: Text
    , password2 :: Text
} deriving Show

-- | Custom form for creating a new account
customNewAccountForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
                              Text.Blaze.Internal.Markup
                              -> Control.Monad.Trans.RWS.Lazy.RWST
                                   (Maybe (Env, FileEnv), HandlerSite m, [Lang])
                                   Enctype
                                   Ints
                                   m
                                   (FormResult CustomNewAccountData, WidgetT (HandlerSite m) IO ())
customNewAccountForm = 
    renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColLg 2) (ColSm 0) (ColLg 10)) $ 
                       CustomNewAccountData <$> areq textField (bfs ("Email" :: Text)) Nothing
                                            <*> areq textField (bfs ("Name" :: Text)) Nothing
                                            <*> areq passwordField (bfs ("Password" :: Text)) Nothing
                                            <*> areq passwordField (bfs ("Confirm Password" :: Text)) Nothing
-- | The registration form
customNewAccountWidget :: YesodAuthAccount db master
                          => (Route Auth -> Route master)
                          -> WidgetT master IO ()
customNewAccountWidget tm = do
    ((_,widget), enctype) <- liftHandlerT $ runFormPost $ customNewAccountForm
    [whamlet|
<div .container>
    <h2> Create an Account
    <div .col-lg-6>
        <div .well.bs-component>
            <form .form-horizontal method=post enctype=#{enctype} action=@{tm newAccountR}>
                ^{widget}
                <input .btn.btn-primary type=submit value=_{Msg.Register}>
|]

-- | Creates a new custom account
createNewCustomAccount :: YesodAuthAccount db master
                          => CustomNewAccountData
                          -> (Route Auth -> Route master)
                          -> HandlerT master IO (AuthRoute)
createNewCustomAccount (CustomNewAccountData email name pwd _) tm = do
    muser <- runAccountDB $ loadUser email
    case muser of
        Just _ -> do setMessageI $ email ++ " already exists."
                     redirect $ tm newAccountR
        Nothing -> return ()

    key <- newVerifyKey
    hashed <- hashPassword pwd

    mnew <- runAccountDB $ addNewUser name email key hashed
    _ <- case mnew of
        Left err -> do setMessage $ toHtml err
                       redirect $ tm newAccountR
        Right x -> return x

    return $ verifyR email key

customLoginForm :: (MonadHandler m, YesodAuthAccount db (HandlerSite m)) =>
                    Markup
                    -> RWST (Maybe (Env, FileEnv), HandlerSite m, [Lang])
                        Enctype Ints m (FormResult LoginData, WidgetT (HandlerSite m) IO ())
customLoginForm = 
    renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColLg 2) (ColSm 0) (ColLg 10)) $ 
    LoginData <$> areq (checkM checkValidUsername textField) (bfs ("Email Address" :: Text)) Nothing
              <*> areq passwordField (bfs ("Password" :: Text)) Nothing

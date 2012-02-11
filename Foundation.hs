module Foundation
    ( FashionAd (..)
    , Route (..)
    , FashionAdMessage (..)
    , resourcesFashionAd
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    ) where

import Prelude
import Yesod
import Yesod.Static
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Auth.Email
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)
#ifdef DEVELOPMENT
import Yesod.Logger (logLazyText)
#endif
import qualified Settings
import qualified Data.ByteString.Lazy as L
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
#if DEVELOPMENT
import qualified Data.Text.Lazy.Encoding
#else
import Network.Mail.Mime (sendmail)
#endif

import qualified Data.Text as T  

--for email
import Data.Text (Text)
import Data.Maybe (isJust)
import Network.Mail.Mime
import Text.Shakespeare.Text (stext)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Hamlet (hamletFile,shamlet)
import Control.Monad (join)
import qualified Data.Text.Lazy.Encoding
--

import Yesod.Form.Jquery
import Yesod.Form.I18n.Japanese()

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data FashionAd = FashionAd
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "FashionAd" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype FashionAdRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route FashionAd = FashionAdRoute
-- * Creates the value resourcesFashionAd which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- FashionAd. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the FashionAdRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "FashionAd" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm FashionAd FashionAd (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod FashionAd where
    approot = ApprootMaster $ appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage
        ma <- maybeAuth
        y <- getYesod

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addScriptEither $ urlJqueryJs y
            addScriptEither $ urlJqueryUiJs y
            addStylesheetEither $ urlJqueryUiCss y
            $(widgetFile "normalize")
            $(widgetFile "default/default-layout")
        hamletToRepHtml $(hamletFile "templates/default/default-layout-wrapper.hamlet")
      where showFullName u | T.null (userFullName u) = "<not set yourname>"
                           | otherwise = userFullName u
    
    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Enable Javascript async loading
    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

-- How to run database actions.
instance YesodPersist FashionAd where
    type YesodPersistBackend FashionAd = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth FashionAd where
    type AuthId FashionAd = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ defaultUser (credsIdent creds)

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ --authOpenId
                      authEmail
                    , authBrowserId
                    , authGoogleEmail
                    ]
    
    authHttpManager = httpManager

--    loginHandler = defaultLayout $ do  
--      $(widgetFile "login")

-- Sends off your mail. Requires sendmail in production!
deliver :: FashionAd -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif

instance YesodAuthEmail FashionAd where
    type AuthEmailId FashionAd = EmailId

    addUnverified email verkey =
        runDB $ insert $ Email email Nothing $ Just verkey

    sendVerifyEmail email _ verurl = do
        y <- getYesod
        liftIO $ deliver y =<< renderMail' defmail
            {
              mailTo = [Address Nothing email],
              mailHeaders =
                [ ("From", "noreply")
                , ("To", email)
                , ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        defmail = emptyMail $ Address Nothing "noreply"
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Please confirm your email address by clicking on the link below.

\#{verurl}

Thank you
|]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [shamlet|
<p>Please confirm your email address by clicking on the link below.
<p>
    <a href=#{verurl}>#{verurl}
<p>Thank you
|]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap emailVerkey) . get
    setVerifyKey eid key = runDB $ update eid [EmailVerkey =. Just key]
    verifyAccount eid = runDB $ do
        me <- get eid
        case me of
            Nothing -> return Nothing
            Just e -> do
                let email = emailEmail e
                case emailUser e of
                    Just uid -> return $ Just uid
                    Nothing -> do
                        uid <- insert$ defaultUser email
                        update eid [EmailUser =. Just uid, EmailVerkey =. Nothing]
                        return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = runDB $ do
        me <- getBy $ UniqueEmail email
        case me of
            Nothing -> return Nothing
            Just (Entity eid e) -> return $ Just EmailCreds
                { emailCredsId = eid
                , emailCredsAuthId = emailUser e
                , emailCredsStatus = isJust $ emailUser e
                , emailCredsVerkey = emailVerkey e
                }
    getEmail = runDB . fmap (fmap emailEmail) . get

instance RenderMessage FashionAd FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery FashionAd

defaultUser :: Text -> User
defaultUser ident = User 
                  { userIdent = ident
                  , userPassword = Nothing
                  , userFullName = ""
                  , userIntroduction = Nothing
                  }

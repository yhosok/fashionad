module Handler.Root where
import Yesod.Auth
import Import
import Text.Julius

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler Html
getRootR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        setTitle "fashionad homepage"
        $(widgetFile "homepage")

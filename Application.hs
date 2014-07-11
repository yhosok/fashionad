{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Client.Conduit (newManager)
import Data.Default (def)
import Control.Monad.Logger (runLoggingT)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import Control.Concurrent (forkIO, threadDelay)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, flushLogStr)
import Network.Wai.Logger (clockDateCacher)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Coordination
import Handler.Item
import Handler.Rating
import Handler.User

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    logWare   = if development then logStdoutDev
                               else logStdout

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConfig)

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, updater) <- clockDateCacher
    
    let updateLoop = do
          threadDelay 1000000
          updater
          flushLogStr loggerSet'
          updateLoop

    _ <- forkIO updateLoop
    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s p manager dbconf logger
    -- Perform database migration using our application's logging settings.
    runLoggingT
      (Database.Persist.runPool dbconf (runMigration migrateAll) p)
      (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

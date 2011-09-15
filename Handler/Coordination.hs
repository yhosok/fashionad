{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Coordination where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Control.Monad (guard)
import qualified Data.Map as M

import Yesod.Form.Jquery
import Foundation
import Handler.Item
import Settings.StaticFiles (js_jquery_simplemodal_js)

coordForm :: UserId -> 
             Maybe Coordination -> 
             Html -> 
             Form FashionAd FashionAd (FormResult Coordination, Widget)
coordForm uid mc = \html -> do
    ruid <- return $ pure uid
    (rtitle,vtitle) <- mreq textField "title" (fmap coordinationTitle mc)
    (rdesc,vdesc) <- mopt textField "description" (fmap coordinationDesc mc)
    mfe <- askFiles
    rcoimg <- return $ chkFile (maybe Nothing (M.lookup "coimg") mfe)
    fmsg <- return $ filemsg rcoimg
    let vs = [vtitle, vdesc]
    return (Coordination <$> ruid <*> rtitle <*> rdesc <*> rcoimg,
            $(widgetFile "coordform"))
  where isExist = (> 0) . L.length . fileContent
        content = B.pack . L.unpack . fileContent
        chkFile (Just fi) | isExist fi = pure (content fi)
                          | otherwise = FormFailure ["missing file"]
        chkFile Nothing = FormMissing
        filemsg (FormFailure [a]) = a
        filemsg _ = ""

getCoordinationsR :: Handler RepHtml
getCoordinationsR = do
  mu <- requireAuth
  cos <- runDB $ selectList [] []
  defaultLayout $ do
    setTitle "fashionad homepage"
    addWidget $(widgetFile "coordinations")

getCoordinationR :: CoordinationId -> Handler RepHtml
getCoordinationR cid = do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  items <- runDB $ selectList [ItemCoordination ==. cid] []
--  mfr <- getFileData "coimg"
  ((res, coordform), enc) <- runFormPost $ coordForm uid mc
  liftIO $ print res
  ((_, itemform), _) <- generateFormPost $ itemForm (Just cid) Nothing
  y <- getYesod
  case res of
    FormSuccess c -> do
      runDB $ replace cid c
      setMessage "Updated Coordination"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  defaultLayout $ do
    addScriptEither $ urlJqueryJs y
    addScript $ StaticR js_jquery_simplemodal_js
    let isNew = False
    let mcid = Just cid
--    let coordform = $(widgetFile "coordform")
    addWidget $(widgetFile "coordination")

-- getFileData :: Text -> Handler (Maybe (B.ByteString, String))
-- getFileData s = do
--   mfi <- lookupFile s
--   return $ fmap (\fi -> (content fi, msg fi))  mfi
--   where isExist = (> 0) . L.length . fileContent
--         content = B.pack . L.unpack . fileContent
--         msg f | not $ isExist f = "nothing image file"
--         msg _  = ""

postCoordinationR :: CoordinationId -> Handler RepHtml
postCoordinationR = getCoordinationR

getAddCoordinationR ::Handler RepHtml
getAddCoordinationR = do
  (uid, u) <- requireAuth
  y <- getYesod
  ((res,coordform),enc) <- runFormPost $ coordForm uid Nothing
  ((_, itemform), _) <- runFormPost $ itemForm Nothing Nothing
  case res of
    FormSuccess c -> do
      cid <- runDB $ insert c
      setMessage "Added new Coordination"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  defaultLayout $ do
    addScriptEither $ urlJqueryJs y
    addScript $ StaticR js_jquery_simplemodal_js
    let isNew = True
    let items = []
    let mc = Nothing
    let mcid = Nothing
    addWidget $(widgetFile "coordination")

postAddCoordinationR :: Handler RepHtml
postAddCoordinationR = getAddCoordinationR

postDelCoordinationR :: CoordinationId -> Handler RepHtml
postDelCoordinationR = undefined

getCoordinationImgR :: CoordinationId -> Handler (ContentType, Content)
getCoordinationImgR cid = do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  case mc of
    Just c -> do
      img <- return $ coordinationImage c
      return (typeJpeg, toContent img)

{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Coordination where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Control.Monad (guard)

import Yesod.Form.Jquery
import Foundation
import Handler.Item
import Settings.StaticFiles (js_jquery_simplemodal_js)

coordForm :: UserId -> 
             Maybe B.ByteString -> 
             Maybe Coordination -> 
             Html -> 
             Form FashionAd FashionAd (FormResult Coordination, Widget)
coordForm uid mf mc = renderTable $ Coordination
    <$> pure uid
    <*> areq textField "title" (fmap coordinationTitle mc)
    <*> aopt textField "description" (fmap coordinationDesc mc)
    <*> pure (maybe B.empty id mf)

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
  mfr <- getFileData "coimg"
  ((res, form), enc) <- runFormPost $ coordForm uid (fst <$> mfr) mc
  ((_, itemForm), _) <- generateFormPost $ itemForm (Just cid) Nothing
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
    let coordform = $(widgetFile "coordform")
    addWidget $(widgetFile "coordination")

getFileData :: Text -> Handler (Maybe (B.ByteString, String))
getFileData s = do
  mfi <- lookupFile s
  return $ fmap (\fi -> (content fi, msg fi))  mfi
  where isExist = (> 0) . L.length . fileContent
        content = B.pack . L.unpack . fileContent
        msg f | not $ isExist f = "nothing image file"
        msg _  = ""

postCoordinationR :: CoordinationId -> Handler RepHtml
postCoordinationR = getCoordinationR

getAddCoordinationR ::Handler RepHtml
getAddCoordinationR = do
  (uid, u) <- requireAuth
  mfr <- getFileData "coimg"
  y <- getYesod
  ((res,form),enc) <- runFormPost $ coordForm uid (fst <$> mfr) Nothing
  ((_, itemForm), _) <- runFormPost $ itemForm Nothing Nothing
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
    let coordform = $(widgetFile "coordform")
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

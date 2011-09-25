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
import Handler.Rating
import Settings.StaticFiles (js_jquery_simplemodal_js)

coordForm :: UserId -> 
             Maybe Coordination -> 
             Html -> 
             Form FashionAd FashionAd (FormResult Coordination, Widget)
coordForm uid mc = \html -> do
    ruid <- return $ pure uid
    (rtitle,vtitle) <- mreq textField 
                       (FieldSettings MsgCoordinationTitle Nothing Nothing Nothing) 
                       (fmap coordinationTitle mc)
    (rdesc,vdesc) <- mopt textField "Description" (fmap coordinationDesc mc)
    mfe <- askFiles
    liftIO $ print mfe
    rcoimg <- return $ chkFile mfe
    fmsg <- return $ filemsg rcoimg
    let vs = [vtitle, vdesc]
    return (Coordination <$> ruid <*> rtitle <*> rdesc <*> rcoimg,
            do addScript $ StaticR js_jquery_simplemodal_js
               $(widgetFile "coordform"))
  where 
    notEmpty = maybe False (not . L.null . fileContent)
    content = maybe B.empty (B.pack . L.unpack . fileContent)
    mfi = M.lookup "coimg"
    chkFile (Just fe) | notEmpty $ mfi fe = pure (content $ mfi fe)
                      | otherwise = maybe (FormFailure ["missing file"]) 
                                          (pure . coordinationImage) mc
    chkFile Nothing = FormMissing
    filemsg (FormFailure [a]) = a
    filemsg _ = ""

coordBaseWidget :: Bool -> Widget -> Widget
coordBaseWidget isNew coordform= $(widgetFile "coordbase")

getCoordinationsR :: Handler RepHtml
getCoordinationsR = do
  mu <- requireAuth
  cos <- runDB $ selectList [] []
  defaultLayout $ do
    setTitle "Fashionad Homepage"
    addWidget $(widgetFile "coordinations")

dispCoordination :: Maybe Widget -> Maybe Widget -> Maybe Widget -> 
                    CoordinationId -> Handler RepHtml
dispCoordination mcf mif mrf cid= do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  (Just c) <- case mc of
    Nothing -> redirect RedirectTemporary $ CoordinationsR
    _ -> return mc
  isMine <- return $ (coordinationUser c) == uid
  items <- runDB $ selectList [ItemCoordination ==. cid] []
  coordbase <- widget (coordBaseWidget False) (coordForm uid mc) mcf
  item <-  widget (itemWidget cid) (itemForm cid Nothing) mif
  mr <- getRating uid cid
  rating <- widget (ratingWidget cid) (ratingForm uid cid (snd <$> mr)) mrf
  y <- getYesod
  defaultLayout $ do
    addScriptEither $ urlJqueryJs y
    addWidget $(widgetFile "coordination")
    addWidget item
    addWidget rating
    addWidget coordbase
  where
    widget wf alt mf = wf <$> (maybe (genForm alt) return mf)
    genForm form = snd . fst <$> (generateFormPost $ form)

getCoordinationR :: CoordinationId -> Handler RepHtml
getCoordinationR cid = do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  case mc of
    Nothing -> redirect RedirectTemporary $ CoordinationsR
    _ -> return ()
  ((res, coordform), enc) <- runFormPost $ coordForm uid mc
  case res of
    FormSuccess c -> do
      runDB $ replace cid c
      setMessage "Updated Coordination"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  dispCoordination (Just coordform) Nothing Nothing cid

postCoordinationR :: CoordinationId -> Handler RepHtml
postCoordinationR = getCoordinationR

getAddCoordinationR ::Handler RepHtml
getAddCoordinationR = do
  (uid, u) <- requireAuth
  y <- getYesod
  ((res,coordform),enc) <- runFormPost $ coordForm uid Nothing
  coordbase <- return $ coordBaseWidget True coordform
  case res of
    FormSuccess c -> do
      cid <- runDB $ insert c
      setMessage "Added new Coordination"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  defaultLayout $ do
    addScriptEither $ urlJqueryJs y
    addWidget coordbase

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

getRatingR :: CoordinationId ->  Handler RepHtml
getRatingR cid = do
  (uid,u) <- requireAuth
  mr <- getRating uid cid
  ratingform <- case mr of
    Nothing -> insRating uid cid
    Just r  -> updRating uid cid r
  dispCoordination Nothing Nothing (Just ratingform) cid

postRatingR :: CoordinationId -> Handler RepHtml
postRatingR = getRatingR

getItemR :: CoordinationId -> ItemId -> Handler RepHtml
getItemR cid iid = do
  (uid,u) <- requireAuth
  mi <- runDB $ get iid
  ((res, itemform), enc) <- runFormPost $ itemForm cid mi
  case res of
    FormSuccess i -> do
      runDB $ replace iid i
      setMessage "Updated Item"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  dispCoordination Nothing (Just itemform) Nothing cid

postItemR :: CoordinationId -> ItemId -> Handler RepHtml
postItemR = getItemR

getAddItemR ::CoordinationId -> Handler RepHtml
getAddItemR cid = do
  (uid, u) <- requireAuth
  ((res,itemform),enc) <- runFormPost $ itemForm cid Nothing
  case res of
    FormSuccess i -> do
      iid <- runDB $ insert i
      setMessage "Added Item"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  dispCoordination Nothing (Just itemform) Nothing cid

postAddItemR :: CoordinationId -> Handler RepHtml
postAddItemR = getAddItemR

postDelItemR :: CoordinationId -> ItemId -> Handler RepHtml
postDelItemR cid iid = do
  (uid, u) <- requireAuth
  runDB $ deleteWhere [ItemId ==. iid]
  redirect RedirectTemporary $ CoordinationR cid

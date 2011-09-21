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
    (rtitle,vtitle) <- mreq textField "title" (fmap coordinationTitle mc)
    (rdesc,vdesc) <- mopt textField "description" (fmap coordinationDesc mc)
    mfe <- askFiles
    liftIO $ print $ maybe Nothing (M.lookup "coimg") mfe
    rcoimg <- return $ chkFile (maybe Nothing (M.lookup "coimg") mfe)
    fmsg <- return $ filemsg rcoimg
    let vs = [vtitle, vdesc]
    return (Coordination <$> ruid <*> rtitle <*> rdesc <*> rcoimg,
            do addScript $ StaticR js_jquery_simplemodal_js
               $(widgetFile "coordform"))
  where notEmpty = not . L.null . fileContent
        content = B.pack . L.unpack . fileContent
        chkFile (Just fi) | notEmpty fi = pure (content fi)
                          | otherwise = FormFailure ["missing file"]
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
    setTitle "fashionad homepage"
    addWidget $(widgetFile "coordinations")

dispCoordination :: Maybe Widget -> Maybe Widget -> Maybe Widget -> 
                    CoordinationId -> Handler RepHtml
dispCoordination mcf mif mrf cid= do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  items <- runDB $ selectList [ItemCoordination ==. cid] []
  coordform <- getCoordForm mcf uid mc
  coordbase <- return $ coordBaseWidget False coordform
  itemform <- getItemForm mif cid  
  mr <- getRating uid cid
  ratingform <- getRatingForm mrf uid cid mr
  y <- getYesod
  defaultLayout $ do
    addScriptEither $ urlJqueryJs y
    addWidget $(widgetFile "coordination")
  where getCoordForm (Just cform) _ _ = return cform
        getCoordForm _ uid mc = genForm (coordForm uid mc)
        getItemForm (Just iform) _ = return iform
        getItemForm _ cid = genForm (itemForm cid Nothing)
        getRatingForm (Just rform) _ _ _ = return rform
        getRatingForm _ uid cid mr = genForm (ratingForm uid cid (snd <$> mr))
        genForm form = snd . fst <$> (generateFormPost $ form)

getCoordinationR :: CoordinationId -> Handler RepHtml
getCoordinationR cid = do
  (uid,u) <- requireAuth
  ((res, coordform), enc) <- runFormPostNoNonce $ coordForm uid Nothing
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
  rs <- runDB $ selectList [RatingUser ==. uid, RatingCoordination ==. cid] [LimitTo 1]
  ratingform <- case rs of
    []-> insRating uid cid
    (r:[]) -> updRating uid cid r
  dispCoordination Nothing Nothing (Just ratingform) cid

postRatingR :: CoordinationId -> Handler RepHtml
postRatingR = getRatingR

getItemR :: CoordinationId -> ItemId -> Handler RepHtml
getItemR cid iid = do
  (uid,u) <- requireAuth
  mi <- runDB $ get iid
  ((res, itemform), enc) <- runFormPostNoNonce $ itemForm cid mi
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
      setMessage "Added new Coordination"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  dispCoordination Nothing (Just itemform) Nothing cid

postAddItemR :: CoordinationId -> Handler RepHtml
postAddItemR = getAddItemR

postDelItemR :: CoordinationId -> ItemId -> Handler RepHtml
postDelItemR cid iid = do
  (uid, u) <- requireAuth
  --i <- runDB $ get iid
  --((_,form),_) <- runFormPost $ itemForm (itemCoordination i) i
  runDB $ deleteWhere [ItemId ==. iid]
  redirect RedirectTemporary $ CoordinationR cid

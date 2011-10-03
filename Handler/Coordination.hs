{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Coordination where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Control.Monad (guard)
import qualified Data.Map as M
import Control.Monad (forM)
import Data.List (genericLength)

import Graphics.GD

import Foundation
import Handler.Item
import Handler.Rating
import Settings.StaticFiles

coordForm :: UserId -> 
             Maybe Coordination -> 
             Html -> 
             Form FashionAd FashionAd (FormResult Coordination, Widget)
coordForm uid mc = \html -> do
    ruid <- return $ pure uid
    (rtitle,vtitle) <- mreq textField 
                       (FieldSettings MsgCoordinationTitle Nothing Nothing Nothing) 
                       (fmap coordinationTitle mc)
    (rdesc,vdesc) <- mopt textareaField "Description" (fmap coordinationDesc mc)
    mfe <- askFiles
    rcoimg <- return $ chkFile mfe
    fmsg <- return $ filemsg rcoimg
    let vs = [vtitle, vdesc]
    return (Coordination <$> ruid <*> rtitle <*> rdesc <*> rcoimg,
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
coordBaseWidget isNew coordform= do
  addCassius $(cassiusFile "coordination")
  $(widgetFile "coordbase")

getCoordinationsR :: Handler RepHtml
getCoordinationsR = do
  mu <- requireAuth
  cs <- runDB $ selectList [] []
  rows <- coordinationList cs
  defaultLayout $ do
    addWidget $(widgetFile "coordinations")

coordinationList :: [(CoordinationId, Coordination)] -> Handler Widget
coordinationList cs = do
  tdata <- forM cs $ \c -> do
    w <- averageRatingWidget (fst c)
    return (c, w)
  return $ mapM_ (\row -> addWidget $(widgetFile "coordlistrow")) $ toRows tdata
  where
    cid = fst . fst
    ctitle = coordinationTitle . snd . fst
    rwidget = snd
    colcnt = 4
    toRows xs = case splitAt colcnt xs of
      (ys,[]) ->  ys:[]
      (ys,zs) ->  ys:toRows zs

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
  defaultLayout $ do
    addWidget $(widgetFile "coordination")
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
    addWidget coordbase

postAddCoordinationR :: Handler RepHtml
postAddCoordinationR = getAddCoordinationR

postDelCoordinationR :: CoordinationId -> Handler RepHtml
postDelCoordinationR = undefined


getCoordinationImgSR, getCoordinationImgLR :: CoordinationId -> Handler (ContentType, Content)
getCoordinationImgSR cid = coordinationImg cid (100,150)
getCoordinationImgLR cid = coordinationImg cid (240,360)

coordinationImg :: CoordinationId -> (Float , Float) -> Handler (ContentType, Content)
coordinationImg cid areaSize = do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  case mc of
    Just c -> do
      cimg <- return $ coordinationImage c 
      resizeImg <- liftIO $ resizeBSImage areaSize cimg
      return (typeJpeg, toContent resizeImg)
    Nothing -> return (typeJpeg, toContent B.empty)

resizeBSImage :: (Float, Float) -> B.ByteString -> IO B.ByteString
resizeBSImage (mw,mh) bsimg = do
  img <- loadJpegByteString bsimg
  (w,h) <- imageSize img
  rimg <- uncurry resizeImage (size w h) $ img 
  saveJpegByteString (-1) rimg
  where
    size w h | realToFrac w > (realToFrac h * mw / mh) = (round mw, calc h w mw)
             | otherwise = (calc w h mh, round mh)
    calc x y max = round $ (realToFrac x / realToFrac y) * max

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

postDelItemR :: CoordinationId -> ItemId -> Handler RepPlain
postDelItemR cid iid = do
  (uid, u) <- requireAuth
  runDB $ deleteWhere [ItemId ==. iid]
  return $ RepPlain $ toContent $ toSinglePiece iid
--  redirect RedirectTemporary $ CoordinationR cid

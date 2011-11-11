module Handler.Coordination where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Monad (guard)
import qualified Data.Map as M
import Control.Monad (forM)
import Data.List (genericLength)

import Graphics.GD

import Import

import Handler.Item
import Handler.Rating
import Handler.Content
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
            $(widgetFile "coordination/coordform"))
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

getCoordinationsR :: Handler RepHtml
getCoordinationsR = do
  (uid, u) <- requireAuth
  coordListPage uid [] $ MsgCoordinationTitle

getMyPageR :: UserId -> Handler RepHtml
getMyPageR uid = do
  requireAuth
  coordListPage uid [CoordinationUser ==. uid] $ MsgMyPage (toSinglePiece uid)

coordListPage :: UserId -> [Filter Coordination] -> 
                 FashionAdMessage -> Handler RepHtml
coordListPage uid filter msgtitle = do
  cs <- runDB $ selectList filter []
  rows <- coordinationList cs
  fashionAdLayout uid $ do
    addWidget $(widgetFile "coordination/coordinations") 

coordinationList :: [(CoordinationId, Coordination)] -> Handler Widget
coordinationList cs = do
  tdata <- forM cs $ \c -> do
    w <- averageRatingWidget (fst c)
    return (c, w)
  return $ mapM_ widget $ toRows tdata
  where
    cid = fst . fst
    ctitle = coordinationTitle . snd . fst
    rwidget = snd
    colcnt = 4
    widget row = addWidget $(widgetFile "coordination/coordlistrow")
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
  coordform <- widget (coordForm uid mc) mcf
  itemform <-  widget (itemForm cid Nothing) mif
  mr <- runDB $ getBy $ UniqueRating uid cid
  ratingform <- widget (ratingForm cid uid (snd <$> mr)) mrf
  fashionAdLayout (coordinationUser c) $ do
    let isNew = False
    $(widgetFile "default/form")
    addWidget $(widgetFile "coordination/coordination")
  where
    widget alt mf = maybe (genForm alt) return mf
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
  ((res,coordform),enc) <- runFormPost $ coordForm uid Nothing
  case res of
    FormSuccess c -> do
      cid <- runDB $ insert c
      setMessage "Added new Coordination"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  fashionAdLayout uid $ do
    $(widgetFile "default/form")
    addWidget $(widgetFile "coordination/newcoordination")

postAddCoordinationR :: Handler RepHtml
postAddCoordinationR = getAddCoordinationR

postDelCoordinationR :: CoordinationId -> Handler RepHtml
postDelCoordinationR = undefined


getCoordinationImgSR,getCoordinationImgLR :: CoordinationId -> Handler (ContentType, Content)
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

getRatingR :: CoordinationId -> UserId -> Handler RepHtml
getRatingR cid uid = do
  requireAuth
  ratingform <- updateRating cid uid
  dispCoordination Nothing Nothing (Just ratingform) cid

postRatingR :: CoordinationId -> UserId -> Handler RepHtml
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

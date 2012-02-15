module Handler.Coordination where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Control.Monad (forM)

import Graphics.GD

import Import
import Handler.Item
import Handler.Rating
import Handler.Content

coordForm :: UserId -> 
             Maybe Coordination -> 
             Form Coordination
coordForm uid mc = \html -> do
    ruid <- return $ pure uid
    (rtitle,vtitle) <- mreq textField 
                       (FieldSettings MsgCoordinationTitle Nothing Nothing Nothing []) 
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
  Entity uid _ <- requireAuth
  coordListPage uid [] $ MsgCoordinationTitle

getMyPageR :: UserId -> Handler RepHtml
getMyPageR uid = do
  requireAuth
  coordListPage uid [CoordinationUser ==. uid] $ MsgMyPage (toPathPiece uid)

coordListPage :: UserId -> [Filter Coordination] -> 
                 FashionAdMessage -> Handler RepHtml
coordListPage uid fc msgtitle = do
  cs <- runDB $ selectList fc []
  rows <- coordinationList cs
  fashionAdLayout uid $ do
    addWidget $(widgetFile "coordination/coordinations") 

coordinationList :: [Entity Coordination] -> Handler Widget
coordinationList cs = do
  tdata <- forM cs $ \c -> do
    w <- averageRatingWidget (entityKey c)
    return (c, w)
  return $ mapM_ widget $ toRows tdata
  where
    cid = entityKey . fst
    ctitle = coordinationTitle . entityVal . fst
    rwidget = snd
    colcnt = 4
    widget row = addWidget $(widgetFile "coordination/coordlistrow")
    toRows xs = case splitAt colcnt xs of
      (ys,[]) ->  ys:[]
      (ys,zs) ->  ys:toRows zs

dispCoordination :: Maybe Widget -> Maybe Widget -> Maybe Widget -> 
                    CoordinationId -> Handler RepHtml
dispCoordination mcf mif mrf cid= do
  Entity uid u <- requireAuth
  mc <- runDB $ get cid
  (Just c) <- case mc of
    Nothing -> redirect  $ CoordinationsR
    _ -> return mc
  isMine <- return $ (coordinationUser c) == uid
  items <- runDB $ selectList [ItemCoordination ==. cid] []
  mr <- runDB $ getBy $ UniqueRating uid cid
  coordform <- isNothingGenForm (coordForm uid mc) mcf
  itemform <-  isNothingGenForm (itemForm cid Nothing) mif
  ratingform <- isNothingGenForm (ratingForm cid uid (entityVal <$> mr)) mrf
  fashionAdLayout (coordinationUser c) $ do
    let isNew = False
    $(widgetFile "default/form")
    addWidget $(widgetFile "coordination/coordination")

isNothingGenForm :: Form a ->
                    Maybe Widget ->
                    Handler Widget
isNothingGenForm alt mf = maybe (genForm alt) return mf
  where genForm form = snd . fst <$> (generateFormPost $ form)

getCoordinationR :: CoordinationId -> Handler RepHtml
getCoordinationR cid = do
  Entity uid _ <- requireAuth
  mc <- runDB $ get cid
  case mc of
    Nothing -> redirect  $ CoordinationsR
    _ -> return ()
  ((res, coordform), _) <- runFormPost $ coordForm uid mc
  case res of
    FormSuccess c -> do
      runDB $ replace cid c
      setMessage "Updated Coordination"
      redirect  $ CoordinationR cid
    _ -> return ()
  dispCoordination (Just coordform) Nothing Nothing cid

postCoordinationR :: CoordinationId -> Handler RepHtml
postCoordinationR = getCoordinationR

getAddCoordinationR ::Handler RepHtml
getAddCoordinationR = do
  Entity uid u <- requireAuth
  ((res,coordform),enc) <- runFormPost $ coordForm uid Nothing
  case res of
    FormSuccess c -> do
      cid <- runDB $ insert c
      setMessage "Added new Coordination"
      redirect  $ CoordinationR cid
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
  requireAuth
  mc <- runDB $ get cid
  case mc of
    Just c -> do
      cimg <- return $ coordinationImage c 
      resizeImg <- liftIO $ resizeBSImage areaSize cimg
      return (typeJpeg, toContent resizeImg)
    Nothing -> return (typeJpeg, toContent B.empty)

resizeBSImage :: (Float, Float) -> B.ByteString -> IO B.ByteString
resizeBSImage (mw,mh) bsimg = do
  img <- loadJpegByteString bsimg `catch` (\err-> loadGifByteString bsimg)
  (w,h) <- imageSize img
  rimg <- uncurry resizeImage (size w h) $ img 
  saveJpegByteString (-1) rimg
  where
    size w h | realToFrac w > (realToFrac h * mw / mh) = (round mw, calc h w mw)
             | otherwise = (calc w h mh, round mh)
    calc x y ml = round $ (realToFrac x / realToFrac y) * ml

getRatingR :: CoordinationId -> UserId -> Handler RepHtml
getRatingR cid uid = do
  requireAuth
  ratingform <- updateRating cid uid
  dispCoordination Nothing Nothing (Just ratingform) cid

postRatingR :: CoordinationId -> UserId -> Handler RepHtml
postRatingR = getRatingR

getItemR :: CoordinationId -> ItemId -> Handler RepHtml
getItemR cid iid = do
  requireAuth
  mi <- runDB $ get iid
  ((res, itemform), _) <- runFormPost $ itemForm cid mi
  case res of
    FormSuccess i -> do
      runDB $ replace iid i
      setMessage "Updated Item"
      redirect  $ CoordinationR cid
    _ -> return ()
  dispCoordination Nothing (Just itemform) Nothing cid

postItemR :: CoordinationId -> ItemId -> Handler RepHtml
postItemR = getItemR

getAddItemR ::CoordinationId -> Handler RepHtml
getAddItemR cid = do
  requireAuth
  ((res,itemform),_) <- runFormPost $ itemForm cid Nothing
  case res of
    FormSuccess i -> do
      _ <- runDB $ insert i
      setMessage "Added Item"
      redirect $ CoordinationR cid
    _ -> return ()
  dispCoordination Nothing (Just itemform) Nothing cid

postAddItemR :: CoordinationId -> Handler RepHtml
postAddItemR = getAddItemR

postDelItemR :: CoordinationId -> ItemId -> Handler RepPlain
postDelItemR _ iid = do
  requireAuth
  runDB $ deleteWhere [ItemId ==. iid]
  return $ RepPlain $ toContent $ toPathPiece iid

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
import Settings.StaticFiles (js_jquery_simplemodal_js,
                             js_jquery_rating_js,
                             css_jquery_rating_css)

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
            $(widgetFile "coordform"))
  where notEmpty = not . L.null . fileContent
        content = B.pack . L.unpack . fileContent
        chkFile (Just fi) | notEmpty fi = pure (content fi)
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
  ((res, coordform), enc) <- runFormPostNoNonce $ coordForm uid mc
  ((_, itemform), _) <- generateFormPost $ itemForm (Just cid) Nothing
  mr <- getRating uid cid
  ((_, ratingform), _) <- generateFormPost $ ratingForm uid (Just cid) (snd <$> mr)
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
    addScript $ StaticR js_jquery_rating_js
    addStylesheet $ StaticR css_jquery_rating_css
    let isNew = False
    let mcid = Just cid
    addWidget $(widgetFile "coordination")

postCoordinationR :: CoordinationId -> Handler RepHtml
postCoordinationR = getCoordinationR

getAddCoordinationR ::Handler RepHtml
getAddCoordinationR = do
  (uid, u) <- requireAuth
  y <- getYesod
  ((res,coordform),enc) <- runFormPost $ coordForm uid Nothing
  ((_, itemform), _) <- runFormPost $ itemForm Nothing Nothing
  ((_, ratingform), _) <- runFormPost $ ratingForm uid Nothing Nothing
  case res of
    FormSuccess c -> do
      cid <- runDB $ insert c
      setMessage "Added new Coordination"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  defaultLayout $ do
    addScriptEither $ urlJqueryJs y
    addScript $ StaticR js_jquery_simplemodal_js
    addScript $ StaticR js_jquery_rating_js
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

getRatingR :: CoordinationId ->  Handler RepHtml
getRatingR cid = do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  items <- runDB $ selectList [ItemCoordination ==. cid] []
  ((_, coordform), _) <- generateFormPost $ coordForm uid Nothing
  ((_, itemform), _) <- generateFormPost $ itemForm (Just cid) Nothing
  rs <- runDB $ selectList [RatingUser ==. uid, RatingCoordination ==. cid] [LimitTo 1]
  ratingform <- case rs of
    []-> insRating uid cid
    (r:[]) -> updRating uid cid r
  y <- getYesod
  defaultLayout $ do
    addScriptEither $ urlJqueryJs y
    addScript $ StaticR js_jquery_simplemodal_js
    addScript $ StaticR js_jquery_rating_js
    addStylesheet $ StaticR css_jquery_rating_css
    let isNew = False
    let mcid = Just cid
    addWidget $(widgetFile "coordination")

getRating :: UserId -> CoordinationId -> Handler (Maybe (RatingId, Rating))
getRating uid cid = do
  rs <- runDB $ selectList [RatingUser ==. uid, RatingCoordination ==. cid] [LimitTo 1]
  case rs of
    []-> return Nothing
    (r:[]) -> return $ Just r

postRatingR :: CoordinationId -> Handler RepHtml
postRatingR = getRatingR

insRating :: UserId -> CoordinationId -> Handler Widget
insRating uid cid = do
  ((res, form), enc) <- runFormPostNoNonce $ ratingForm uid (Just cid) Nothing
  case res of
    FormSuccess r -> do
      rid <- runDB $ insert  r
      setMessage "Add Rating"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return form

updRating :: UserId -> CoordinationId -> (RatingId, Rating) -> Handler Widget
updRating uid cid (rid,r) = do
  ((res, form), enc) <- runFormPostNoNonce $ ratingForm uid (Just cid) (Just r)
  case res of
    FormSuccess r' -> do
      runDB $ replace rid r'
      setMessage "Updated Item"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return $ form

getItemR :: CoordinationId -> ItemId -> Handler RepHtml
getItemR cid iid = do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  items <- runDB $ selectList [ItemCoordination ==. cid] []
  ((_, coordform), _) <- generateFormPost $ coordForm uid Nothing
  ((_, ratingform), _) <- runFormPost $ ratingForm uid Nothing Nothing
  mi <- runDB $ get iid
  ((res, itemform), enc) <- runFormPostNoNonce $ itemForm (Just cid) mi
  case res of
    FormSuccess i -> do
      runDB $ replace iid i
      setMessage "Updated Item"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  y <- getYesod
  defaultLayout $ do
    addScriptEither $ urlJqueryJs y
    addScript $ StaticR js_jquery_simplemodal_js
    addScript $ StaticR js_jquery_rating_js
    addStylesheet $ StaticR css_jquery_rating_css
    let isNew = False
    let mcid = Just cid
    addWidget $(widgetFile "coordination")

postItemR :: CoordinationId -> ItemId -> Handler RepHtml
postItemR = getItemR

getAddItemR ::CoordinationId -> Handler RepHtml
getAddItemR cid = do
  (uid, u) <- requireAuth
  mc <- runDB $ get cid
  items <- runDB $ selectList [ItemCoordination ==. cid] []
  ((_, coordform), _) <- generateFormPost $ coordForm uid Nothing
  ((_, ratingform), _) <- runFormPost $ ratingForm uid Nothing Nothing
  ((res,itemform),enc) <- runFormPost $ itemForm (Just cid) Nothing
  case res of
    FormSuccess i -> do
      iid <- runDB $ insert i
      setMessage "Added new Coordination"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()
  y <- getYesod
  defaultLayout $ do
    addScriptEither $ urlJqueryJs y
    addScript $ StaticR js_jquery_simplemodal_js
    addScript $ StaticR js_jquery_rating_js
    addStylesheet $ StaticR css_jquery_rating_css
    let isNew = False
    let mcid = Just cid
    addWidget $(widgetFile "coordination")

postAddItemR :: CoordinationId -> Handler RepHtml
postAddItemR = getAddItemR

postDelItemR :: CoordinationId -> ItemId -> Handler RepHtml
postDelItemR cid iid = do
  (uid, u) <- requireAuth
  --i <- runDB $ get iid
  --((_,form),_) <- runFormPost $ itemForm (itemCoordination i) i
  runDB $ deleteWhere [ItemId ==. iid]
  redirect RedirectTemporary $ CoordinationR cid

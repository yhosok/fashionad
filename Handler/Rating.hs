{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleContexts#-}
module Handler.Rating where

import Control.Applicative
import Data.Text (Text, pack)

import Foundation
import Settings.StaticFiles (js_jquery_rating_js,
                             css_jquery_rating_css)

ratingForm :: UserId -> CoordinationId -> Maybe Rating -> Html -> Form FashionAd FashionAd (FormResult Rating, Widget)
ratingForm uid cid mr= \html -> do
  ruid <- return $ pure uid
  rcid <- return $ pure cid
  (rrating, vrating) <- mreq (starField rateValues) "Rate" (ratingValue <$> mr)
  liftIO $ print rrating
  (rcomment, vcomment) <- mopt textField "Comment" (ratingComment <$> mr)
  return (Rating <$> ruid <*> rcid <*> rrating <*> rcomment, 
          do addScript $ StaticR js_jquery_rating_js
             addStylesheet $ StaticR css_jquery_rating_css
             $(widgetFile "ratingform"))

rateValues :: [(Text, Int)]
rateValues = map (\x -> (pack $ show x, x)) [1..5]

starField :: (Eq a, Show a, RenderMessage master FormMessage) =>
             [(Text, a)] -> Field sub master a
starField opts = Field
  { fieldParse = return . starParser
  , fieldView = \theId name val isReq -> [whamlet|\
   $forall v <- values
     <input id=#{theId}-#{v} type=radio name=#{name} value=#{v} .star :isSel v val:checked>
|]
  }
 where
    values = map fst opts
    starParser [] = Right Nothing
    starParser (x:_) = case x of
      "" -> Right Nothing
      x  -> case lookup x opts of
              Just y -> Right $ Just y
              Nothing -> Left $ SomeMessage $ MsgInvalidEntry x 
    isSel v = either (const False) ((==v) . pack . show)

ratingWidget :: CoordinationId -> Widget -> Widget
ratingWidget cid ratingform = $(widgetFile "rating")

getRatingsR :: CoordinationId -> Handler RepHtml
getRatingsR = undefined

getRating :: UserId -> CoordinationId -> Handler (Maybe (RatingId, Rating))
getRating uid cid = do
  rs <- runDB $ selectList [RatingUser ==. uid, RatingCoordination ==. cid]
                           [LimitTo 1]
  case rs of
    []-> return Nothing
    (r:[]) -> return $ Just r

insRating :: UserId -> CoordinationId -> Handler Widget
insRating uid cid = do
  ((res, form), enc) <- runFormPost $ ratingForm uid cid Nothing
  case res of
    FormSuccess r -> do
      rid <- runDB $ insert  r
      setMessage "Add Rating"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return form

updRating :: UserId -> CoordinationId -> (RatingId, Rating) -> Handler Widget
updRating uid cid (rid,r) = do
  ((res, form), enc) <- runFormPost $ ratingForm uid cid (Just r)
  case res of
    FormSuccess r' -> do
      runDB $ replace rid r'
      setMessage "Updated Rating"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return $ form

postDelRatingR :: CoordinationId -> RatingId -> Handler RepHtml
postDelRatingR cid rid = do
  (uid, u) <- requireAuth
  runDB $ deleteWhere [RatingId ==. rid]
  redirect RedirectTemporary $ CoordinationR cid

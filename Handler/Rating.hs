{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Rating where

import Control.Applicative
import Data.Text (Text, pack)

import Foundation

{--
ratingForm :: UserId -> CoordinationId ->  Maybe Rating -> Html -> Form FashionAd FashionAd (FormResult Rating, Widget)
ratingForm uid cid mr= renderDivs $ Rating
  <$> pure uid
  <*> pure cid
  <*> areq (radioField rateValues) "Rate" (ratingValue <$> mr)
  <*> aopt textField "Comment" (ratingComment <$> mr)
--}  
ratingForm :: UserId -> Maybe CoordinationId ->  Maybe Rating -> Html -> Form FashionAd FashionAd (FormResult Rating, Widget)
ratingForm uid mcid mr= \html -> do
  ruid <- return $ pure uid
  rcid <- return $ maybe FormMissing pure mcid
  (rvalue, vvalue) <- mreq (radioField rateValues) "Rate" (ratingValue <$> mr)
  (rcomment, vcomment) <- mopt textField "Comment" (ratingComment <$> mr)
  let vs = [vvalue, vcomment]
  return (Rating <$> ruid <*> rcid <*> rvalue <*> rcomment, 
          $(widgetFile "ratingform"))

rateValues :: [(Text, Rate)]
rateValues = map (\x -> (pack $ show x, x)) [minBound..maxBound]

getRatingsR :: CoordinationId -> Handler RepHtml
getRatingsR = undefined

getRatingR :: CoordinationId ->  Handler ()
getRatingR cid = do
  (uid,u) <- requireAuth
  rs <- runDB $ selectList [RatingUser ==. uid, RatingCoordination ==. cid] [LimitTo 1]
  case rs of
    []-> insRating uid cid
    (r:[]) -> updRating uid cid r

getRating :: UserId -> CoordinationId -> Handler (Maybe (RatingId, Rating))
getRating uid cid = do
  rs <- runDB $ selectList [RatingUser ==. uid, RatingCoordination ==. cid] [LimitTo 1]
  case rs of
    []-> return Nothing
    (r:[]) -> return $ Just r

postRatingR :: CoordinationId -> Handler ()
postRatingR = getRatingR

insRating :: UserId -> CoordinationId -> Handler ()
insRating uid cid = do
  ((res, form), enc) <- runFormPostNoNonce $ ratingForm uid (Just cid) Nothing
  case res of
    FormSuccess r -> do
      rid <- runDB $ insert  r
      setMessage "Add Rating"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()

updRating :: UserId -> CoordinationId -> (RatingId, Rating) -> Handler ()
updRating uid cid (rid,r) = do
  ((res, form), enc) <- runFormPostNoNonce $ ratingForm uid (Just cid) (Just r)
  case res of
    FormSuccess r' -> do
      runDB $ replace rid r'
      setMessage "Updated Item"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()


-- getAddRatingR ::CoordinationId -> Handler ()
-- getAddRatingR cid = do
--   (uid, u) <- requireAuth
--   ((res,form),enc) <- runFormPost $ ratingForm uid cid Nothing
--   case res of
--     FormSuccess r -> do
--       rid <- runDB $ insert r
--       setMessage "Added new Coordination"
--       redirect RedirectTemporary $ CoordinationR cid
--     _ -> return ()

-- postAddRatingR :: CoordinationId -> Handler ()
-- postAddRatingR = getAddRatingR

postDelRatingR :: CoordinationId -> RatingId -> Handler RepHtml
postDelRatingR cid rid = do
  (uid, u) <- requireAuth
  --i <- runDB $ get iid
  --((_,form),_) <- runFormPost $ itemForm (itemCoordination i) i
  runDB $ deleteWhere [RatingId ==. rid]
  redirect RedirectTemporary $ CoordinationR cid

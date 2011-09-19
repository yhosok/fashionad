{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleContexts#-}
module Handler.Rating where

import Control.Applicative
import Data.Text (Text, pack)

import Foundation

ratingForm :: UserId -> Maybe CoordinationId ->  Maybe Rating -> Html -> Form FashionAd FashionAd (FormResult Rating, Widget)
ratingForm uid mcid mr= \html -> do
  ruid <- return $ pure uid
  rcid <- return $ maybe FormMissing pure mcid
  (rrating, vrating) <- mreq (starField rateValues) "Rate" (ratingValue <$> mr)
  liftIO $ print rrating
  (rcomment, vcomment) <- mopt textField "Comment" (ratingComment <$> mr)
  return (Rating <$> ruid <*> rcid <*> rrating <*> rcomment, 
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

getRatingsR :: CoordinationId -> Handler RepHtml
getRatingsR = undefined

-- getRatingR :: CoordinationId ->  Handler ()
-- getRatingR cid = do
--   (uid,u) <- requireAuth
--   rs <- runDB $ selectList [RatingUser ==. uid, RatingCoordination ==. cid] [LimitTo 1]
--   case rs of
--     []-> insRating uid cid
--     (r:[]) -> updRating uid cid r

-- getRating :: UserId -> CoordinationId -> Handler (Maybe (RatingId, Rating))
-- getRating uid cid = do
--   rs <- runDB $ selectList [RatingUser ==. uid, RatingCoordination ==. cid] [LimitTo 1]
--   case rs of
--     []-> return Nothing
--     (r:[]) -> return $ Just r

-- postRatingR :: CoordinationId -> Handler ()
-- postRatingR = getRatingR

-- insRating :: UserId -> CoordinationId -> Handler ()
-- insRating uid cid = do
--   ((res, form), enc) <- runFormPostNoNonce $ ratingForm uid (Just cid) Nothing
--   case res of
--     FormSuccess r -> do
--       rid <- runDB $ insert  r
--       setMessage "Add Rating"
--       redirect RedirectTemporary $ CoordinationR cid
--     _ -> return ()

-- updRating :: UserId -> CoordinationId -> (RatingId, Rating) -> Handler ()
-- updRating uid cid (rid,r) = do
--   ((res, form), enc) <- runFormPostNoNonce $ ratingForm uid (Just cid) (Just r)
--   case res of
--     FormSuccess r' -> do
--       runDB $ replace rid r'
--       setMessage "Updated Item"
--       redirect RedirectTemporary $ CoordinationR cid
--     _ -> return ()


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

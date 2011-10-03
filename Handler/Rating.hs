{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleContexts#-}
module Handler.Rating where

import Control.Applicative
import Data.Text (Text, pack, append)
import Data.List (genericLength)

import Foundation
import Settings.StaticFiles

ratingForm :: CoordinationId -> UserId -> Maybe Rating -> 
              Html -> Form FashionAd FashionAd (FormResult Rating, Widget)
ratingForm cid uid mr= \html -> do
  ruid <- return $ pure uid
  rcid <- return $ pure cid
  (rrating, vrating) <- mreq (starField rates) "Rate" (ratingValue <$> mr)
  liftIO $ print rrating
  (rcomment, vcomment) <- mopt textareaField "Comment" (ratingComment <$> mr)
  return (Rating <$> ruid <*> rcid <*> rrating <*> rcomment, 
          do addScript $ StaticR js_jquery_rating_js
             addStylesheet $ StaticR css_jquery_rating_css
             $(widgetFile "ratingform"))

rateValues = [1..5]

rates :: [(Text, Int)]
rates = map (\x -> (pack $ show x, x)) rateValues

starField :: (Eq a, Show a, RenderMessage master FormMessage) =>
             [(Text, a)] -> Field sub master a
starField opts = Field
  { fieldParse = return . starParser
  , fieldView = \theId name val isReq -> 
      starWidget (op name) val values isSel
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
    op n = StarOp {name = n, isDisabled = False, split = Nothing}

ratingWidget :: CoordinationId -> UserId -> Widget -> Widget
ratingWidget cid uid ratingform = $(widgetFile "rating")

averageRatingWidget :: CoordinationId -> Handler Widget
averageRatingWidget cid = do
  rs <- runDB $ selectList [RatingCoordination ==. cid][]
  return $ do
    addScript $ StaticR js_jquery_metadata_js
    addScript $ StaticR js_jquery_rating_js
    addStylesheet $ StaticR css_jquery_rating_css
    starWidget op (roundVal rs) values isSel
  where
    rates = map (ratingValue . snd) 
    average xs = realToFrac (sum xs) / genericLength xs
    name = append "rating-" . toSinglePiece
    split = 4
    values = map (pack . show) [1..(last rateValues * split)]
    roundVal = round . (*) (fromIntegral split) . average . rates
    isSel v = (==v) . (pack . show)
    op = StarOp {name = (name cid), isDisabled = True, split = Just split}

data StarOptions =  StarOp { name :: Text, 
                             isDisabled :: Bool,
                             split :: Maybe Int }

starWidget :: StarOptions -> a -> [Text] -> (Text -> a -> Bool) -> 
              GWidget sub master ()
starWidget op val values isSel = [whamlet|\
$forall v <- values
    <input type="radio" name=#{name op} class=#{clstr} value=#{v} :isDisabled op:disabled :isSel v val:checked>
|]
  where 
    clstr = "star" `append` (maybe "" splitstr (split op))
    splitstr i = " {split:" `append` (pack $ show i) `append` "}"

getRatingsR :: CoordinationId -> Handler RepHtml
getRatingsR = undefined

getRating :: UserId -> CoordinationId -> Handler (Maybe (RatingId, Rating))
getRating uid cid = do
  rs <- runDB $ selectList [RatingUser ==. uid, RatingCoordination ==. cid]
                           [LimitTo 1]
  case rs of
    []-> return Nothing
    (r:[]) -> return $ Just r

insRating :: CoordinationId -> UserId -> Handler Widget
insRating cid uid = do
  ((res, form), enc) <- runFormPost $ ratingForm cid uid Nothing
  case res of
    FormSuccess r -> do
      rid <- runDB $ insert  r
      setMessage "Add Rating"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return form

updRating :: CoordinationId -> UserId -> (RatingId, Rating) -> Handler Widget
updRating cid uid (rid,r) = do
  ((res, form), enc) <- runFormPost $ ratingForm cid uid (Just r)
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

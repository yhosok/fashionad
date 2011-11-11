{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, FlexibleContexts#-}
module Handler.Rating where

import Data.Text (Text, pack, append)
import Data.List (genericLength)

import Import
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
          $(widgetFile "ratingform"))

rateValues = [1..5]

rates :: [(Text, Int)]
rates = map (\x -> (pack $ show x, x)) rateValues

starField :: (Eq a, Show a, RenderMessage FashionAd FormMessage) =>
             [(Text, a)] -> Field sub FashionAd a
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

averageRatingWidget :: CoordinationId -> Handler Widget
averageRatingWidget cid = do
  rs <- runDB $ selectList [RatingCoordination ==. cid][]
  return $ do
    starWidget op (roundVal rs) values isSel
  where
    rates = map (ratingValue . snd) 
    average xs = realToFrac (sum xs) / genericLength xs
    name = append "rating-" . toSinglePiece
    split = 4
    values = map (pack . show) [1..(last rateValues * split)]
    roundVal = round . (*) (fromIntegral split) . average . rates
    isSel v = (==v) . pack . show
    op = StarOp {name = name cid, isDisabled = True, split = Just split}

data StarOptions =  StarOp { name :: Text, 
                             isDisabled :: Bool,
                             split :: Maybe Int }

starWidget :: StarOptions -> a -> [Text] -> (Text -> a -> Bool) -> 
              GWidget sub FashionAd ()
starWidget op val values isSel = do
  addScript $ StaticR js_jquery_metadata_js
  addScript $ StaticR js_jquery_rating_js
  addStylesheet $ StaticR css_jquery_rating_css
  addJulius [julius|$.fn.rating.options.required = true;|]
  addWidget [whamlet|\
$forall v <- values
    <input type="radio" name=#{name op} class=#{clstr} value=#{v} :isDisabled op:disabled :isSel v val:checked>
|]
  where 
    clstr = "star" `append` (maybe "" splitstr (split op))
    splitstr i = " {split:" `append` (pack $ show i) `append` "}"

getRatingsR :: CoordinationId -> Handler RepHtml
getRatingsR = undefined

updateRating :: CoordinationId -> UserId -> Handler Widget
updateRating cid uid = do
  mr <- runDB $ getBy $ UniqueRating uid cid
  ((res, form), enc) <- runFormPost $ ratingForm cid uid (snd <$> mr)
  case res of
    FormSuccess r -> do
      case mr of 
        Nothing -> do runDB $ insert  r
                      setMessage "Add Rating"
        Just (rid,_) -> do runDB $ replace rid r
                           setMessage "Updated Rating"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return form

postDelRatingR :: CoordinationId -> RatingId -> Handler RepHtml
postDelRatingR cid rid = do
  (uid, u) <- requireAuth
  runDB $ deleteWhere [RatingId ==. rid]
  redirect RedirectTemporary $ CoordinationR cid

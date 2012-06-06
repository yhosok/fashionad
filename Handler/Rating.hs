module Handler.Rating where

import Data.Text (pack, append)
import Data.List (genericLength)

import Import
import Settings.StaticFiles

ratingForm :: CoordinationId -> UserId -> Maybe Rating -> Form Rating
ratingForm cid uid mr= \html -> do
  ruid <- return $ pure uid
  rcid <- return $ pure cid
  (rrating, vrating) <- mreq (starField rates) "Rate" (ratingValue <$> mr)
  (rcomment, vcomment) <- mopt textareaField
                          (toSettings MsgRateComment)
                          (ratingComment <$> mr)
  return (Rating <$> ruid <*> rcid <*> rrating <*> rcomment, 
          $(widgetFile "coordination/ratingform"))

rateValues :: [Int]
rateValues = [1..5]

rates :: [(Text, Int)]
rates = map (\x -> (pack $ show x, x)) rateValues

starField :: (Eq a, Show a) => [(Text, a)] -> Field sub FashionAd a
starField opts = Field
  { fieldParse = return . starParser
  , fieldView = \_ nm _ val req -> 
      starWidget (op nm) val values isSel
  }
 where
    values = map fst opts
    starParser [] = Right Nothing
    starParser (x:_) = case x of
      "" -> Right Nothing
      x'  -> case lookup x' opts of
              Just y -> Right $ Just y
              Nothing -> Left $ SomeMessage $ MsgInvalidEntry x'
    isSel v = either (const False) ((==v) . pack . show)
    op n = StarOp {name = n, isDisabled = False, split = Nothing}

averageRatingWidget :: CoordinationId -> Handler Widget
averageRatingWidget cid = do
  rs <- runDB $ selectList [RatingCoordination ==. cid][]
  return $ do
    starWidget op (roundVal rs) values isSel
  where
    roundVal :: [Entity Rating] -> Int
    roundVal = round . (*) (fromIntegral split') . average . rvs
    average :: [Int] -> Double
    average xs = realToFrac (sum xs) / genericLength xs
    rvs = map (ratingValue . entityVal) 
    nm = append "rating-" . toPathPiece
    split' = 4
    values = map (pack . show) [1..(last rateValues * split')]
    isSel v = (==v) . pack . show
    op = StarOp {name = nm cid, isDisabled = True, split = Just split'}

data StarOptions =  StarOp { name :: Text, 
                             isDisabled :: Bool,
                             split :: Maybe Int }

starWidget :: StarOptions -> a -> [Text] -> (Text -> a -> Bool) -> 
              GWidget sub FashionAd ()
starWidget op val values isSel = do
  addScript $ StaticR js_jquery_metadata_js
  addScript $ StaticR js_jquery_rating_js
  addStylesheet $ StaticR css_jquery_rating_css
  toWidget [julius|$.fn.rating.options.required = true;|]
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
  ((res, form), _) <- runFormPost $ ratingForm cid uid (entityVal <$> mr)
  case res of
    FormSuccess r -> do
      case mr of 
        Nothing -> do _ <- runDB $ insert  r
                      setMessage "Add Rating"
        Just (Entity rid _) -> do runDB $ replace rid r
                                  setMessage "Updated Rating"
      redirect $ CoordinationR cid
    _ -> return form

postDelRatingR :: CoordinationId -> RatingId -> Handler RepHtml
postDelRatingR cid rid = do
  requireAuth
  runDB $ deleteWhere [RatingId ==. rid]
  redirect $ CoordinationR cid

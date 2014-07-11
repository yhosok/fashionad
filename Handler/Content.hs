module Handler.Content where

import Yesod.Auth
import Import

userInfoWidget :: UserId -> Handler Widget
userInfoWidget uid = do
  mu <- maybeAuth
  (muser,following,follower,coord) <- runDB $ (,,,)
                                      <$> get uid
                                      <*> count [FollowFollower ==. uid]
                                      <*> count [FollowFollowed ==. uid]
                                      <*> count [CoordinationUser ==. uid]
  follow <- followWidget uid
  return $(widgetFile "user/userinfo")
  where isMyInfo = (==uid) . entityKey

fashionAdLayout :: UserId -> Widget -> Handler Html
fashionAdLayout uid main = do
  sidebar <- userInfoWidget uid
  defaultLayout $ do
    $(widgetFile "default/content-layout")

followWidget :: UserId -> Handler Widget
followWidget uid = do
  lid <- requireAuthId
  mf <- runDB $ getBy $ UniqueFollow lid uid
  return $(widgetFile "user/follow")
  where uidtxt = toPathPiece uid
        isFollow = maybe False (const True)

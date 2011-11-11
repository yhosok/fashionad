{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Content where

import Control.Applicative

import Foundation

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
  where isMyInfo = (==uid) . fst

fashionAdLayout :: UserId -> Widget -> Handler RepHtml
fashionAdLayout uid main = do
  sidebar <- userInfoWidget uid
  defaultLayout $ do
    $(widgetFile "default/content-layout")

followWidget :: UserId -> Handler Widget
followWidget uid = do
  (lid,_) <- requireAuth
  mf <- runDB $ getBy $ UniqueFollow lid uid
  return $(widgetFile "user/follow")
  where uidtxt = toSinglePiece uid
        isFollow = maybe False (const True)

{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Content where

import Control.Applicative

import Foundation

userInfoWidget :: UserId -> Handler Widget
userInfoWidget uid = do
  mu <- maybeAuth
  (following,follower,coord) <- runDB $ (,,)
                                <$> count [FollowFollower ==. uid]
                                <*> count [FollowFollowed ==. uid]
                                <*> count [CoordinationUser ==. uid]
  follow <- followWidget uid
  return $(widgetFile "userinfo")
  where isMyInfo = ((==uid) . fst)


fashionAdLayout :: UserId -> Widget -> Handler RepHtml
fashionAdLayout uid main = do
  sidebar <- userInfoWidget uid
  defaultLayout $ do
    $(widgetFile "content-layout")

followWidget :: UserId -> Handler Widget
followWidget uid = do
  (uid',u') <- requireAuth
  mf <- runDB $ getBy $ UniqueFollow uid' uid
  return $(widgetFile "follow")
  where uidtxt = toSinglePiece uid
        isFollow = maybe False (const True)

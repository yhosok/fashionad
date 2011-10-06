{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Content where

import Control.Applicative

import Foundation

userInfoWidget :: UserId -> Widget
userInfoWidget uid = do
  (following,follower,coord) <- lift . runDB $ (,,)
                                <$> count [FollowFollower ==. uid]
                                <*> count [FollowFollowed ==. uid]
                                <*> count [CoordinationUser ==. uid]
  $(widgetFile "userinfo")
  
fashionAdLayout uid main = 
  defaultLayout $ do
    let sidebar = userInfoWidget uid
    $(widgetFile "content-layout")
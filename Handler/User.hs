{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.User where

import Control.Applicative
import Control.Monad (forM)
import qualified Data.Text as T (Text, pack, empty)

import Database.Persist.Join (SelectOneMany (..), selectOneMany)
import Database.Persist.Join.Sql (runJoin)

import Foundation
import Handler.Content

postFollowR :: Handler RepPlain
postFollowR = do
  (uid,u) <- requireAuth
  followHelper uid $ \fid -> insert $ Follow uid fid

postUnFollowR :: Handler RepPlain
postUnFollowR = do
  (uid,u) <- requireAuth
  followHelper uid $ \fid -> deleteBy $ UniqueFollow uid fid

followHelper :: UserId -> (UserId -> YesodDB FashionAd FashionAd a) 
                -> Handler RepPlain
followHelper uid dbop = do
  mid <- runInputPost $ fmap fromSinglePiece $ ireq hiddenField "followuid"
  case mid of
    Just fid -> do
      runDB $ dbop fid
      return $ RepPlain $ toContent $ toSinglePiece fid
    Nothing -> return $ RepPlain $ toContent T.empty

getFollowingR :: UserId -> Handler RepHtml
getFollowingR uid = do
  requireAuth
  us <- runDB $ selectList [FollowFollower ==. uid] []
  userListPage uid [UserId <-. (map (followFollowed . snd) us)]

getFollowersR :: UserId -> Handler RepHtml
getFollowersR uid = do
  requireAuth
  us <- runDB $ selectList [FollowFollowed ==. uid] []
  userListPage uid [UserId <-. (map (followFollower . snd) us)]

getUsersR :: Handler RepHtml
getUsersR = do
  (uid,u) <- requireAuth
  userListPage uid []
  
userListPage :: UserId -> [Filter User] -> Handler RepHtml
userListPage uid filter = do
  us <- runDB $ selectList filter []  
  users <- forM us $ \user@(uid',_) -> 
    fmap (\fw -> (user,fw)) $ followWidget uid'
  fashionAdLayout uid $(widgetFile "users")
  where ident = userIdent . snd . fst
        isMyInfo = (==uid) . fst . fst

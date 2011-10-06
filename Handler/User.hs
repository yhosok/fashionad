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
  liftIO $ print mid
  case mid of
    Just fid -> do
      runDB $ dbop fid
      return $ RepPlain $ toContent $ toSinglePiece fid
    Nothing -> return $ RepPlain $ toContent T.empty

getFollowingR :: UserId -> Handler RepHtml
getFollowingR uid = do
  requireAuth
  us <- runDB $ selectList [FollowFollower ==. uid] []
  userListPage [UserId <-. (map (followFollowed . snd) us)]

getFollowersR :: UserId -> Handler RepHtml
getFollowersR uid = do
  requireAuth
  us <- runDB $ selectList [FollowFollowed ==. uid] []
  userListPage [UserId <-. (map (followFollower . snd) us)]

getUsersR :: Handler RepHtml
getUsersR = do
  (uid,u) <- requireAuth
  userListPage []
  
userListPage :: [Filter User] -> Handler RepHtml
userListPage filter = do
  (uid,u) <- requireAuth
  users <- runDB $ do
        us <- selectList filter []
        forM us $ \user@(uid',u') -> do 
          mf <- getBy $ UniqueFollow uid uid'
          return (user, mf)
  fashionAdLayout uid $(widgetFile "users")
  where ident = userIdent . snd . fst
        uidtxt = toSinglePiece . fst . fst
        isFollow = maybe False (const True) . snd
        isSameUser u uid = (fst . fst $ u) == uid



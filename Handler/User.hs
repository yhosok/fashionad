{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.User where

import Control.Applicative
import Control.Monad (forM)
import qualified Data.Text as T (Text, pack, empty)

import Foundation

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
  liftIO $ print "!!!!!!!!!!!!!!!!!!!!!!!!!"
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
  us <- runDB $ selectList filter []
  users <- forM us $ \user@(uid',u') -> do 
    mf <- runDB $ getBy $ UniqueFollow uid uid'
    return (user, mf)
  defaultLayout $(widgetFile "users")
  where ident = userIdent . snd . fst
        uidtxt = toSinglePiece . fst . fst
        isFollow = maybe False (const True) . snd

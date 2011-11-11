{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.User where

import Control.Monad (forM)
import qualified Data.Text as T (Text, pack, empty)

import Database.Persist.Join (SelectOneMany (..), selectOneMany)
import Database.Persist.Join.Sql (runJoin)

import Import
import Handler.Content

userForm :: User -> Html -> Form FashionAd FashionAd (FormResult User, Widget)
userForm u = renderDivs $ User
  <$> pure (userIdent u)
  <*> pure (userPassword u)
  <*> areq textField "Full name" (Just $ userFullName u)
  <*> aopt textareaField "Introduction" (Just $ userIntroduction u)

getProfileR :: Handler RepHtml
getProfileR = do
  (uid,u) <- requireAuth
  ((res,userform),enc) <- runFormPost $ userForm u
  case res of
    FormSuccess u' -> do
      runDB $ replace uid u'
      setMessage "Update Your Profile"
      redirect RedirectTemporary $ CoordinationsR
    _ -> return ()
  fashionAdLayout uid $ do
    $(widgetFile "user/form")
    addWidget $(widgetFile "user/profile")

postProfileR :: Handler RepHtml
postProfileR = getProfileR

getUserR :: UserId -> Handler RepHtml
getUserR uid = do
  requireAuth
-- why internal error from get404 ???
--  u <- runDB $ get404 uid
  u <- do
    mu <- runDB $ get uid
    case mu of
      Just u -> return u
      Nothing -> notFound
  fashionAdLayout uid $ do
    addWidget $(widgetFile "user/user")

postFollowR :: Handler RepPlain
postFollowR = do
  (uid,u) <- requireAuth
  mfuid <- runInputPost $ fmap fromSinglePiece $ ireq hiddenField "followuid"
  case mfuid of
    Nothing -> return $ RepPlain $ toContent T.empty
    Just fuid -> do
      mf <- runDB $ getBy $ UniqueFollow uid fuid
      case mf of
        Just (fid,f) -> runDB $ delete fid >> retKey fuid
        Nothing -> (runDB $ insert $ Follow  uid fuid) >> retKey fuid
  where retKey fuid = return $ RepPlain $ toContent $ toSinglePiece fuid

{--
postFollowR' :: Handler RepPlain
postFollowR' = do
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
--}

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
  fashionAdLayout uid $(widgetFile "user/users")
  where ident = userIdent . snd . fst
        isMyInfo = (==uid) . fst . fst

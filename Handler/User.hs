module Handler.User where

import Yesod.Auth
import Control.Monad (forM)
import qualified Data.Text as T (empty)

--import Database.Persist.Join (SelectOneMany (..), selectOneMany)
--import Database.Persist.Join.Sql (runJoin)

import Import
import Handler.Content

userForm :: User -> Form User
userForm u = renderDivs $ User
  <$> pure (userIdent u)
  <*> pure (userPassword u)
  <*> areq textField "Full name" (Just $ userFullName u)
  <*> aopt textareaField "Introduction" (Just $ userIntroduction u)

getProfileR :: Handler Html
getProfileR = do
  Entity uid u <- requireAuth
  ((res,userform),enc) <- runFormPost $ userForm u
  case res of
    FormSuccess u' -> do
      runDB $ replace uid u'
      setMessage "Update Your Profile"
      redirect CoordinationsR
    _ -> return ()
  fashionAdLayout uid $ do
    $(widgetFile "user/profile")

postProfileR :: Handler Html
postProfileR = getProfileR

getUserR :: UserId -> Handler Html
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
    $(widgetFile "user/user")

postFollowR :: Handler RepPlain
postFollowR = do
  uid <- requireAuthId
  mfuid <- runInputPost $ fmap fromPathPiece $ ireq hiddenField "followuid"
  case mfuid of
    Nothing -> return $ RepPlain $ toContent T.empty
    Just fuid -> do
      mf <- runDB $ getBy $ UniqueFollow uid fuid
      case mf of
        Just (Entity fid _) -> (runDB $ delete fid) >> retKey fuid
        Nothing -> (runDB $ insert $ Follow  uid fuid) >> retKey fuid
  where retKey fuid = return $ RepPlain $ toContent $ toPathPiece fuid

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

getFollowingR :: UserId -> Handler Html
getFollowingR uid = do
  requireAuth
  us <- runDB $ selectList [FollowFollower ==. uid] []
  userListPage uid [UserId <-. (map (followFollowed . entityVal) us)]

getFollowersR :: UserId -> Handler Html
getFollowersR uid = do
  requireAuth
  us <- runDB $ selectList [FollowFollowed ==. uid] []
  userListPage uid [UserId <-. (map (followFollower . entityVal) us)]

getUsersR :: Handler Html
getUsersR = do
  uid <- requireAuthId
  userListPage uid []
  
userListPage :: UserId -> [Filter User] -> Handler Html
userListPage uid fu = do
  us <- runDB $ selectList fu []  
  users <- forM us $ \user@(Entity uid' _) -> 
    fmap (\fw -> (user,fw)) $ followWidget uid'
  fashionAdLayout uid $(widgetFile "user/users")
  where ident = userIdent . entityVal . fst
        isMyInfo = (==uid) . entityKey . fst

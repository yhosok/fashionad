{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Item where

import Control.Applicative
import Data.Text (Text, pack)

import Foundation

itemForm :: CoordinationId -> Maybe Item -> Html -> Form FashionAd FashionAd (FormResult Item, Widget)
itemForm cid mi = renderTable $ Item
    <$> areq textField "name" (fmap itemName mi)
    <*> pure cid
    <*> areq (selectField kinds) "kind" (fmap itemKind mi)
    <*> aopt textField "link" (fmap itemLink mi)
    <*> aopt intField "price" (fmap itemPrice mi)
                    
kinds :: [(Text, Kind)]
kinds = map (\x -> (pack $ show x, x)) [minBound..maxBound]

getItemsR :: CoordinationId -> Handler RepHtml
getItemsR = undefined

getItemR :: CoordinationId -> ItemId -> Handler ()
getItemR cid iid = do
  (uid,u) <- requireAuth
  mi <- runDB $ get iid
  ((res, form), enc) <- runFormPostNoNonce $ itemForm cid mi
  case res of
    FormSuccess i -> do
      runDB $ replace iid i
      setMessage "Updated Item"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()

postItemR :: CoordinationId -> ItemId -> Handler ()
postItemR = getItemR

getAddItemR ::CoordinationId -> Handler ()
getAddItemR cid = do
  (uid, u) <- requireAuth
  ((res,form),enc) <- runFormPost $ itemForm cid Nothing
  case res of
    FormSuccess i -> do
      iid <- runDB $ insert i
      setMessage "Added new Coordination"
      redirect RedirectTemporary $ CoordinationR cid
    _ -> return ()

postAddItemR :: CoordinationId -> Handler ()
postAddItemR = getAddItemR

postDelItemR :: CoordinationId -> ItemId -> Handler RepHtml
postDelItemR cid iid = do
  (uid, u) <- requireAuth
  --i <- runDB $ get iid
  --((_,form),_) <- runFormPost $ itemForm (itemCoordination i) i
  runDB $ deleteWhere [ItemId ==. iid]
  redirect RedirectTemporary $ CoordinationR cid
  

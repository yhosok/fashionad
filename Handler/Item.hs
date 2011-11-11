{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Item where

import Control.Applicative
import Data.Text (Text, pack)

import Foundation

itemForm :: CoordinationId -> Maybe Item -> Html -> Form FashionAd FashionAd (FormResult Item, Widget)
itemForm cid mi = \html -> do
  (rname, vname) <- mreq textField "name" (fmap itemName mi)
  rcid <- return $ pure cid
  (rkind, vkind) <- mreq (selectField kinds) "kind" (fmap itemKind mi)
  (rlink, vlink) <- mopt urlField "link" (fmap itemLink mi)
  (rprice, vprice) <- mopt priceField "price" (fmap itemPrice mi)
  let vs = [vname,vkind,vlink,vprice]
  return (Item <$> rname <*> rcid <*> rkind <*> rlink <*> rprice,
          $(widgetFile "coordination/itemform"))
  where priceField = check valPrice intField
        valPrice p | p < 0 = Left priceErrorMsg
                   | otherwise = Right p
        priceErrorMsg :: Text
        priceErrorMsg = "Price is too small."

kinds :: [(Text, Kind)]
kinds = map (\x -> (pack $ show x, x)) [minBound..maxBound]

getItemsR :: CoordinationId -> Handler RepHtml
getItemsR = undefined

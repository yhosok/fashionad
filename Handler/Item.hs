module Handler.Item where

import Data.Text (pack)

import Import
import ModelTypes

itemForm :: CoordinationId -> Maybe Item -> Form Item
itemForm cid mi = \html -> do
  (rname, vname) <- mreq textField (toSettings MsgItemName) (fmap itemName mi)
  rcid <- return $ pure cid
  (rkind, vkind) <- mreq (selectFieldList kinds)
                    (toSettings MsgItemKind)
                    (fmap itemKind mi)
  (rlink, vlink) <- mopt urlField
                    (toSettings MsgItemLink)
                    (fmap itemLink mi)
  (rprice, vprice) <- mopt priceField
                      (toSettings MsgItemPrice)
                      (fmap itemPrice mi)
  let vs = [vname,vkind,vlink,vprice]
  return (Item <$> rname <*> rcid <*> rkind <*> rlink <*> rprice,
          $(widgetFile "coordination/itemform"))
  where priceField = check valPrice intField
        valPrice p | p < 0 = Left priceErrorMsg
                   | otherwise = Right p
--        priceErrorMsg :: Text
        priceErrorMsg = MsgTooSmallPrice

kinds :: [(Text, Kind)]
kinds = map (\x -> (pack $ show x, x)) [minBound..maxBound]

getItemsR :: CoordinationId -> Handler Html
getItemsR = undefined

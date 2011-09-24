{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.String (IsString (..))
import Data.Text (Text)

data FashionAdMessage = MsgFashionAdTitle 
                      | MsgCoordinationList
                      | MsgNoCoordinationList
                      | MsgCoordination
                      | MsgItemList
                      | MsgAddItem
                      | MsgBaseInfo
                      | MsgAddCoordination
                      | MsgModifyBaseInfo
                      | MsgYourRating
                      | MsgCoordinationTitle


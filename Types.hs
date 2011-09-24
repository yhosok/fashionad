{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Text (Text)

data FashionAdMessage = MsgFashionAdTitle 
                      | MsgCoordinationList
                      | MsgCoordination
                      | MsgItemList
                      | MsgAddItem
                      | MsgBaseInfo

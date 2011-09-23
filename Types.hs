{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Text (Text)

data FashionAdMessage = FashionAdTitle 
                      | CoordinationList
                      | Coordination
                      | ItemList
                      | AddItem
                      | BaseInfo

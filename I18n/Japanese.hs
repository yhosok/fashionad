{-# LANGUAGE OverloadedStrings #-}
module I18n.Japanese where

import Data.Monoid (mappend)
import Data.Text (Text)

import Types (FashionAdMessage (..))

japaneseMessage :: FashionAdMessage -> Text
japaneseMessage MsgFashionAdTitle = "ファッションコーディネート"
japaneseMessage MsgCoordinationList = "コーディネート一覧"
japaneseMessage MsgItemList = "アイテム一覧"
japaneseMessage MsgAddItem = "アイテム追加"
japaneseMessage MsgBaseInfo = "基本情報"

englishMessage :: FashionAdMessage -> Text
englishMessage MsgFashionAdTitle = "FashionAd"
englishMessage MsgCoordinationList = "Coordination List"

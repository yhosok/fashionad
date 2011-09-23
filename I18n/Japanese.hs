{-# LANGUAGE OverloadedStrings #-}
module I18n.Japanese where

import Data.Monoid (mappend)
import Data.Text (Text)

import Types (FashionAdMessage (..))

japaneseMessage :: FashionAdMessage -> Text
japaneseMessage FashionAdTitle = "ファッションコーディネート"
japaneseMessage CoordinationList = "コーディネート一覧"
japaneseMessage ItemList = "アイテム一覧"
japaneseMessage AddItem = "アイテム追加"
japaneseMessage BaseInfo = "基本情報"

englishMessage :: FashionAdMessage -> Text
englishMessage FashionAdTitle = "FashionAd"
englishMessage CoordinationList = "Coordination List"

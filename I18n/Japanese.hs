{-# LANGUAGE OverloadedStrings #-}
module I18n.Japanese where

import Data.Monoid (mappend)
import Data.Text (Text)

import Types (FashionAdMessage (..))

japaneseMessage :: FashionAdMessage -> Text
japaneseMessage MsgFashionAdTitle = "ファッションコーディネート"
japaneseMessage MsgCoordinationList = "コーディネート一覧"
japaneseMessage MsgNoCoordinationList = "コーディネートの登録がありません。"
japaneseMessage MsgItemList = "アイテム一覧"
japaneseMessage MsgAddItem = "アイテム追加"
japaneseMessage MsgBaseInfo = "基本情報"
japaneseMessage MsgAddCoordination = "新規登録"
japaneseMessage MsgModifyBaseInfo = "基本情報編集"
japaneseMessage MsgYourRating = "あなたの評価"
japaneseMessage MsgCoordinationTitle = "タイトル"

englishMessage :: FashionAdMessage -> Text
englishMessage MsgFashionAdTitle = "FashionAd"
englishMessage MsgCoordinationList = "Coordination List"
englishMessage MsgNoCoordinationList = "There is no coordination list."
englishMessage MsgItemList = "Item List"
englishMessage MsgAddItem = "Add Item"
englishMessage MsgBaseInfo = "Base Info"
englishMessage MsgAddCoordination = "Add New Coordination"
englishMessage MsgModifyBaseInfo = "Modify Base Info"
englishMessage MsgYourRating = "Your Rating"
englishMessage MsgCoordinationTitle = "Title"
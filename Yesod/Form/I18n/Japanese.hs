{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Japanese where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

japaneseFormMessage :: FormMessage -> Text
japaneseFormMessage (MsgInvalidInteger t) = "不正な数値です: " `mappend` t
japaneseFormMessage (MsgInvalidNumber t) = "不正な数値です: " `mappend` t
japaneseFormMessage (MsgInvalidEntry t) = "不正な値です: " `mappend` t
japaneseFormMessage MsgInvalidTimeFormat = "時刻の入力が不正です。 HH:MM[:SS] の形で入力してください。"
japaneseFormMessage MsgInvalidDay = "日付の入力が不正です。YYYY-MM-DD の形式で入力してください。"
japaneseFormMessage (MsgInvalidUrl t) = "不正なURLです: " `mappend` t
japaneseFormMessage (MsgInvalidEmail t) = "不正なEmailアドレスです: " `mappend` t
japaneseFormMessage (MsgInvalidHour t) = "時の入力が不正です: " `mappend` t
japaneseFormMessage (MsgInvalidMinute t) = "分の入力が不正です: " `mappend` t
japaneseFormMessage (MsgInvalidSecond t) = "秒の入力が不正です: " `mappend` t
japaneseFormMessage MsgCsrfWarning = "クロスサイトリクエストからの保護のため送信できませんでした。フォーム送信内容を確認して下さい。"
japaneseFormMessage MsgValueRequired = "必須入力です"
japaneseFormMessage (MsgInputNotFound t) = "必須入力です: " `mappend` t
japaneseFormMessage MsgSelectNone = "未選択"
japaneseFormMessage (MsgInvalidBool t) = "不正なBool値です: " `mappend` t
japaneseFormMessage MsgBoolYes = "はい"
japaneseFormMessage MsgBoolNo = "いいえ"
japaneseFormMessage MsgDelete = "削除してよろしいですか?"

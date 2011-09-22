{-# LANGUAGE OverloadedStrings #-}
module I18n.Japanese where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

japaneseFormMessage :: FormMessage -> Text
japaneseFormMessage (MsgInvalidInteger t) = "Invalid integer: " `mappend` t
japaneseFormMessage (MsgInvalidNumber t) = "Invalid number: " `mappend` t
japaneseFormMessage (MsgInvalidEntry t) = "Invalid entry: " `mappend` t
japaneseFormMessage MsgInvalidTimeFormat = "Invalid time, must be in HH:MM[:SS] format"
japaneseFormMessage MsgInvalidDay = "Invalid day, must be in YYYY-MM-DD format"
japaneseFormMessage (MsgInvalidUrl t) = "Invalid URL: " `mappend` t
japaneseFormMessage (MsgInvalidEmail t) = "Invalid e-mail address: " `mappend` t
japaneseFormMessage (MsgInvalidHour t) = "Invalid hour: " `mappend` t
japaneseFormMessage (MsgInvalidMinute t) = "Invalid minute: " `mappend` t
japaneseFormMessage (MsgInvalidSecond t) = "Invalid second: " `mappend` t
japaneseFormMessage MsgCsrfWarning = "As a protection against cross-site request forgery attacks, please confirm your form submission."
japaneseFormMessage MsgValueRequired = "Value is required"
japaneseFormMessage (MsgInputNotFound t) = "Input not found: " `mappend` t
japaneseFormMessage MsgSelectNone = "<None>"
japaneseFormMessage (MsgInvalidBool t) = "Invalid boolean: " `mappend` t
japaneseFormMessage MsgBoolYes = "Yes"
japaneseFormMessage MsgBoolNo = "No"
japaneseFormMessage MsgDelete = "Delete?"

data FashionAdMessage = FashionAdTitle | CoordinationList

japaneseMessage FashionAdTitle = "ファッションコーディネート"
japaneseMessage CoordinationList = "コーディネート一覧"

englishMessage FashionAdTitle = "FashionAd"
englishMessage CoordinationList = "Coordination List"

{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.User where

import Control.Applicative
import Data.Text (Text, pack)

import Foundation

getFollowingR :: UserId -> Handler RepHtml
getFollowingR = undefined

postFollowingR :: UserId -> Handler RepHtml
postFollowingR = undefined

getFollowersR :: UserId -> Handler RepHtml
getFollowersR = undefined

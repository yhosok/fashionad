module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , (<>)
    , Text
    , module Data.Monoid
    , module Control.Applicative
    , toSettings
    ) where

import Prelude hiding (writeFile, readFile)
import Yesod   hiding (Route(..))
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

toSettings msg = FieldSettings
    { fsLabel = SomeMessage msg
    , fsTooltip = Nothing
    , fsId = Nothing
    , fsName = Nothing
    , fsAttrs = []
    }
    
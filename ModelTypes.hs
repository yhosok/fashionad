module ModelTypes where

import Prelude
import Yesod

data Rate = Good | Better | Normal | NotGood | Bad
          deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Rate"

data Kind = Hat | Shirt | Pants | Inner
                  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Kind"

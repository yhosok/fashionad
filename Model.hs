module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.ByteString
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
data Rate = Good | Better | Normal | NotGood | Bad
          deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Rate"

data Kind = Hat | Shirt | Pants | Inner
                  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Kind"

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

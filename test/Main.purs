module Test.Main where

import Database.Postgres
import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Cont.Trans
import Control.Monad.Trans
import Data.Array
import Data.Foldable
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Index


main = do
  exampleUsingWithConnection
  exampleLowLevel

data Artist = Artist
  { name :: String
  , year :: Number
  }

connectionInfoTest =
  { host: "localhost"
  , db: "test"
  , port: 5432
  , user: "testuser"
  , password: "test"
  }


-- An example of bracketing using withConnectionCont. --------------------------

exampleUsingWithConnection :: forall eff. DBEff (trace :: Trace | eff) Unit
exampleUsingWithConnection =
  runContT
    (withConnection $ runQueryCont "select * from artist")
    printArtists


withConnection = withConnectionCont connectionInfoTest


-- An example showing some of the low-level functions. ------------------------

exampleLowLevel :: forall eff. DBEff (trace :: Trace | eff) Unit
exampleLowLevel = do
  client <- connect connectionInfoTest

  runQuery "select * from artist order by name asc" client printArtists

  -- run a final query and end the client
  runQuery "select * from artist order by name desc" client $ \artists -> do
    printArtists artists
    endClient client

  -- connect and execute queries
  connectClient client

--------------------------------------------------------------------------------

printArtists :: forall eff. [F Artist] -> Eff (trace :: Trace | eff) Unit
printArtists artists = trace $ "result:\n" <> foldMap stringify artists
  where stringify = show >>> flip (<>) "\n"

instance artistShow :: Show Artist where
  show (Artist p) = "Artist (" <> p.name <> ", " <> show p.year <> ")"

instance artistIsForeign :: IsForeign Artist where
  read obj = do
    n <- readProp "name" obj
    y <- readProp "year" obj
    return $ Artist { name: n, year: y }

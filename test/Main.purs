module Test.Main where

import Database.Postgres
import Debug.Trace
import Control.Monad.Eff
import Data.Array
import Data.Foldable
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Index


main = do
  client <- connect { host: "localhost"
                    , db: "test"
                    , port: 5432
                    , user: "testuser"
                    , password: "test"
                    }

  runQuery client "select * from artist" printArtists

  -- run a final query and end the client
  runQuery client "select * from artist" $ \artists -> do
    printArtists artists
    endClient client

  -- connect and execute queries
  connectClient client


printArtists :: forall eff. [F Artist] -> Eff (trace :: Trace | eff) Unit
printArtists artists = trace $ "result:\n" <> foldMap stringify artists
  where stringify = show >>> flip (<>) "\n"


data Artist = Artist
  { name :: String
  , year :: Number
  }

instance artistShow :: Show Artist where
  show (Artist p) = "Artist (" <> p.name <> ", " <> show p.year <> ")"

instance artistIsForeign :: IsForeign Artist where
  read obj = do
    n <- readProp "name" obj
    y <- readProp "year" obj
    return $ Artist { name: n, year: y }

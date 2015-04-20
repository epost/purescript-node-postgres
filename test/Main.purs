module Test.Main where

import Database.Postgres
import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Cont.Trans
import Control.Monad.Trans
import Data.Array
import Data.Foldable
import Data.Either
import Data.Maybe
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Index
import Control.Monad.Aff


main = runAff (trace <<< show) (const $ trace "All ok") $ do
  exampleUsingWithConnection
  exampleLowLevel

  res <- attempt exampleError
  liftEff $ either (const $ trace "got an error, like we should") (const $ trace "FAIL") res

data Artist = Artist
  { name :: String
  , year :: Number
  }

connectionInfo =
  { host: "localhost"
  , db: "test"
  , port: 5432
  , user: "testuser"
  , password: "test"
  }

exampleUsingWithConnection :: forall eff. Aff (trace :: Trace, db :: DB | eff) Unit
exampleUsingWithConnection = withConnection connectionInfo $ \c -> do
  execute (Query "delete from artist") c
  execute (Query "insert into artist values ('Led Zeppelin', 1968)") c
  execute (Query "insert into artist values ('Deep Purple', 1968)") c
  year <- queryValue (Query "insert into artist values ('Fairport Convention', 1967) returning year" :: Query Number) c
  liftEff $ print (show year)
  artists <- query (Query "select * from artist" :: Query Artist) c
  liftEff $ printRows artists

exampleLowLevel :: forall eff. Aff (trace :: Trace, db :: DB | eff) Unit
exampleLowLevel = do
  client <- connect connectionInfo
  artists <- query (Query "select * from artist order by name desc" :: Query Artist) client
  liftEff $ printRows artists
  liftEff $ end client

exampleError :: forall eff. Aff (db :: DB | eff) (Maybe Artist)
exampleError = withConnection connectionInfo $ \c -> do
  execute (Query "delete from artist") c
  execute (Query "insert into artist values ('Led Zeppelin', 1968)") c
  queryOne (Query "select year from artist") c

printRows :: forall a eff. (Show a) => [a] -> Eff (trace :: Trace | eff) Unit
printRows rows = trace $ "result:\n" <> foldMap stringify rows
  where stringify = show >>> flip (<>) "\n"

instance artistShow :: Show Artist where
  show (Artist p) = "Artist (" <> p.name <> ", " <> show p.year <> ")"

instance artistIsForeign :: IsForeign Artist where
  read obj = do
    n <- readProp "name" obj
    y <- readProp "year" obj
    return $ Artist { name: n, year: y }

module Test.Main where

import Prelude
import qualified Control.Monad.Eff.Console as C
import Control.Monad.Aff.Console (log, print)

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Cont.Trans
import Control.Monad.Trans
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Data.Array
import Data.Foldable
import Data.Either
import Data.Maybe
import Data.Foreign
import Data.Foreign.Class
import Control.Monad.Aff

import Database.Postgres
import Database.Postgres.SqlValue
import Database.Postgres.Transaction

main = runAff C.print (const $ C.log "All ok") $ do
  print $ "connecting to " <> mkConnectionString connectionInfo <> "..."
  exampleUsingWithConnection
  exampleLowLevel

  res <- attempt exampleError
  either (const $ log "got an error, like we should") (const $ log "FAIL") res

  exampleQueries

  exampleTransaction

  liftEff $ disconnect

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

exampleUsingWithConnection :: forall eff. Aff (console :: C.CONSOLE, db :: DB | eff) Unit
exampleUsingWithConnection = withConnection connectionInfo $ \c -> do
  execute_ (Query "delete from artist") c
  execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
  execute_ (Query "insert into artist values ('Deep Purple', 1968)") c
  year <- queryValue_ (Query "insert into artist values ('Fairport Convention', 1967) returning year" :: Query Number) c
  print (show year)
  artists <- query_ (Query "select * from artist" :: Query Artist) c
  printRows artists

exampleLowLevel :: forall eff. Aff (console :: C.CONSOLE, db :: DB | eff) Unit
exampleLowLevel = do
  client <- connect connectionInfo
  artists <- query_ (Query "select * from artist order by name desc" :: Query Artist) client
  printRows artists
  liftEff $ end client

exampleError :: forall eff. Aff (db :: DB | eff) (Maybe Artist)
exampleError = withConnection connectionInfo $ \c -> do
  execute_ (Query "delete from artist") c
  execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
  queryOne_ (Query "select year from artist") c

exampleQueries :: forall eff. Aff (console :: C.CONSOLE, db :: DB | eff) Unit
exampleQueries = withClient connectionInfo $ \c -> do
  log "Example queries with params:"
  execute_ (Query "delete from artist") c
  execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
  execute_ (Query "insert into artist values ('Deep Purple', 1968)") c
  execute_ (Query "insert into artist values ('Toto', 1977)") c
  artists <- query (Query "select * from artist where name = $1" :: Query Artist) [toSql "Toto"] c
  printRows artists

exampleTransaction :: forall eff. Aff (console :: C.CONSOLE, db :: DB | eff) Unit
exampleTransaction = withConnection connectionInfo $ \c -> do
  execute_ (Query "delete from artist") c
  apathize $ tryInsert c
  one <- queryOne_ (Query "select * from artist" :: Query Artist) c
  void $ print one
    where
    tryInsert = withTransaction $ \c -> do
      execute_ (Query "insert into artist values ('Not there', 1999)") c
      throwError $ error "fail"

printRows :: forall a eff. (Show a) => Array a -> Aff (console :: C.CONSOLE | eff) Unit
printRows rows = void $ log $ "result:\n" <> foldMap stringify rows
  where stringify = show >>> flip (<>) "\n"

instance artistShow :: Show Artist where
  show (Artist p) = "Artist (" <> p.name <> ", " <> show p.year <> ")"

instance artistIsForeign :: IsForeign Artist where
  read obj = do
    n <- readProp "name" obj
    y <- readProp "year" obj
    return $ Artist { name: n, year: y }

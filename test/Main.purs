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
    (withConnection $ \c -> do
      runQueryCont_ (Query "delete from artist") c
      runQueryCont_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
      runQueryCont_ (Query "insert into artist values ('Deep Purple', 1968)") c
      year <- runQueryCont (Query "insert into artist values ('Fairport Convention', 1967) returning year" :: Query Number) c
      lift $ print (show year)
      runQueryCont (Query "select * from artist" :: Query Artist) c
    )
    printRows

withConnection = withConnectionCont connectionInfoTest

-- An example showing some of the low-level functions. ------------------------

exampleLowLevel :: forall eff. DBEff (trace :: Trace | eff) Unit
exampleLowLevel = do
  client <- connect connectionInfoTest

  runQuery (Query "select * from artist order by name asc" :: Query Artist) client printRows

  -- run a final query and end the client
  runQuery (Query "select * from artist order by name desc" :: Query Artist) client $ \artists -> do
    printRows artists
    endClient client

  -- connect and execute queries
  connectClient client

--------------------------------------------------------------------------------

printRows :: forall a eff. (Show a) => [F a] -> Eff (trace :: Trace | eff) Unit
printRows rows = trace $ "result:\n" <> foldMap stringify rows
  where stringify = show >>> flip (<>) "\n"

instance artistShow :: Show Artist where
  show (Artist p) = "Artist (" <> p.name <> ", " <> show p.year <> ")"

instance artistIsForeign :: IsForeign Artist where
  read obj = do
    n <- readProp "name" obj
    y <- readProp "year" obj
    return $ Artist { name: n, year: y }

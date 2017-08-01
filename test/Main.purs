module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, apathize, attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Error.Class (throwError)
import Data.Array (length)
import Data.Date (canonicalDate)
import Data.Date.Component (Month(..))
import Data.DateTime (DateTime(..))
import Data.Either (either)
import Data.Enum (toEnum)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index (readProp)
import Data.Generic (class Generic, gEq)
import Data.JSDate (toDateTime)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Time (Time(..))
import Database.Postgres (DB, Query(Query), connect, end, execute, execute_, mkConnectionString, query, queryOne_, queryValue_, query_, withClient, withConnection)
import Database.Postgres.SqlValue (toSql)
import Database.Postgres.Transaction (withTransaction)
import Node.Process (PROCESS)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Unsafe.Coerce (unsafeCoerce)

data Artist = Artist
  { name :: String
  , year :: Int
  }

connectionInfo :: { host :: String, db :: String, port :: Int, user :: String, password :: String }
connectionInfo =
  { host: "localhost"
  , db: "test"
  , port: 5432
  , user: "testuser"
  , password: "test"
  }

main :: forall eff.
  Eff
    ( console :: CONSOLE
    , timer :: TIMER
    , avar :: AVAR
    , process :: PROCESS
    , db :: DB
    | eff
    )
    Unit
main = run [consoleReporter] do
  describe "connection string" do
    it "should build one from the connection record" do
      mkConnectionString connectionInfo `shouldEqual` "postgres://testuser:test@localhost:5432/test"

  describe "withConnection" do
    it "Returns a connection" do
      withConnection connectionInfo $ \c -> do
        execute_ (Query "delete from artist") c
        execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
        execute_ (Query "insert into artist values ('Deep Purple', 1968)") c
        let
          q :: Query Int
          q = Query "insert into artist values ('Fairport Convention', 1967) returning year"

        year <- queryValue_ q c
        year `shouldEqual` (Just 1967)

        artists <- query_ (Query "select * from artist" :: Query Artist) c
        length artists `shouldEqual` 3

  describe "Low level API" do
    it "Can be used to manage connections manually" do
      client <- connect connectionInfo
      execute_ (Query "delete from artist") client
      execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") client

      artists <- query_ (Query "select * from artist order by name desc" :: Query Artist) client
      artists `shouldEqual` [Artist { name: "Led Zeppelin", year: 1968 }]

      liftEff $ end client

  describe "Error handling" do
    it "When query cannot be converted to the requested data type we get an error" do
      res <- attempt exampleError
      either (const $ pure unit) (const $ fail "FAIL") res

  describe "Query params" do
    it "Select using a query param" do
      withClient connectionInfo $ \c -> do
        execute_ (Query "delete from artist") c
        execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
        execute_ (Query "insert into artist values ('Deep Purple', 1968)") c
        execute_ (Query "insert into artist values ('Toto', 1977)") c
        artists <- query (Query "select * from artist where name = $1" :: Query Artist) [toSql "Toto"] c
        length artists `shouldEqual` 1

        noRows <- query (Query "select * from artist where name = $1" :: Query Artist) [toSql "FAIL"] c
        length noRows `shouldEqual` 0

  describe "data types" do
    it "datetimes can be inserted" do
      withConnection connectionInfo \c -> do
        execute_ (Query "delete from types") c
        let date = canonicalDate <$> toEnum 2016 <*> Just January <*> toEnum 25
            time = Time <$> toEnum 23 <*> toEnum 1 <*> toEnum 59 <*> toEnum 0
            dt = DateTime <$> date <*> time
        maybe (fail "Not a datetime") (\ts -> do
          execute (Query "insert into types(timestamp_no_tz) VALUES ($1)") [toSql ts] c
          ts' <- queryValue_ (Query "select timestamp_no_tz at time zone 'UTC' from types" :: Query Foreign) c
          let res = unsafeCoerce <$> ts' >>= toDateTime
          res `shouldEqual` (Just ts)
        ) dt


  describe "transactions" do
    it "does not commit after an error inside a transation" do
      withConnection connectionInfo $ \c -> do
        execute_ (Query "delete from artist") c
        apathize $ tryInsert c
        one <- queryOne_ (Query "select * from artist" :: Query Artist) c

        one `shouldEqual` Nothing
          where
          tryInsert = withTransaction $ \c -> do
            execute_ (Query "insert into artist values ('Not there', 1999)") c
            throwError $ error "fail"

exampleError :: forall eff. Aff (db :: DB | eff) (Maybe Artist)
exampleError = withConnection connectionInfo $ \c -> do
  execute_ (Query "delete from artist") c
  execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
  queryOne_ (Query "select year from artist") c

instance artistShow :: Show Artist where
  show (Artist p) = "Artist (" <> p.name <> ", " <> show p.year <> ")"

derive instance genericArtist :: Generic Artist

instance eqArtist :: Eq Artist where
  eq = gEq

instance artistIsForeign :: Decode Artist where
  decode obj = do
    n <- decode =<< readProp "name" obj
    y <- decode =<< readProp "year" obj
    pure $ Artist { name: n, year: y }

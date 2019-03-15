module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (length)
import Data.Bifunctor (lmap)
import Data.Date (canonicalDate)
import Data.Date.Component (Month(..))
import Data.DateTime (DateTime(..))
import Data.Either (Either, either)
import Data.Enum (toEnum)
import Data.JSDate (toDateTime)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Time (Time(..))
import Database.Postgres (Query(Query), connect, end, execute, execute_, query, queryOne_, queryValue_, query_, withClient, ClientConfig, ConnectionInfo, connectionInfoFromConfig, defaultPoolConfig, mkPool, release)
import Database.Postgres.SqlValue (toSql)
import Database.Postgres.Transaction (withTransaction)
import Effect (Effect)
import Effect.Aff (Aff, Error, apathize, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign (Foreign)
import Simple.JSON as JSON
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Unsafe.Coerce (unsafeCoerce)

type Artist =
  { name :: String
  , year :: Int
  }

clientConfig :: ClientConfig
clientConfig =
  { host: "localhost"
  , database: "test"
  , port: 5432
  , user: "testuser"
  , password: "test"
  , ssl: false
  }

connectionInfo :: ConnectionInfo
connectionInfo = connectionInfoFromConfig clientConfig defaultPoolConfig

read' :: forall a. JSON.ReadForeign a => Foreign -> Either Error a
read' = lmap (error <<< show) <<< JSON.read

main :: Effect Unit
main = run [consoleReporter] do
  describe "withClient" do
    it "Returns a client" do
      pool <- liftEffect $ mkPool connectionInfo
      withClient pool $ \c -> do
        execute_ (Query "delete from artist") c
        execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
        execute_ (Query "insert into artist values ('Deep Purple', 1968)") c
        let
          q :: Query Int
          q = Query "insert into artist values ('Fairport Convention', 1967) returning year"

        year <- queryValue_ read' q c
        year `shouldEqual` (Just 1967)

        artists <- query_ read' (Query "select * from artist" :: Query Artist) c
        length artists `shouldEqual` 3
        liftEffect $ end pool

  describe "Low level API" do
    it "Can be used to manage connections manually" do
      pool <- liftEffect $ mkPool connectionInfo
      client <- connect pool
      execute_ (Query "delete from artist") client
      execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") client

      artists <- query_ read' (Query "select * from artist order by name desc" :: Query Artist) client
      artists `shouldEqual` [{ name: "Led Zeppelin", year: 1968 }]

      liftEffect $ release client
      liftEffect $ end pool

  describe "Error handling" do
    it "When query cannot be converted to the requested data type we get an error" do
      res <- attempt exampleError
      either (const $ pure unit) (const $ fail "FAIL") res

  describe "Query params" do
    it "Select using a query param" do
      pool <- liftEffect $ mkPool connectionInfo
      withClient pool $ \c -> do
        execute_ (Query "delete from artist") c
        execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
        execute_ (Query "insert into artist values ('Deep Purple', 1968)") c
        execute_ (Query "insert into artist values ('Toto', 1977)") c
        artists <- query read' (Query "select * from artist where name = $1" :: Query Artist) [toSql "Toto"] c
        length artists `shouldEqual` 1

        noRows <- query read' (Query "select * from artist where name = $1" :: Query Artist) [toSql "FAIL"] c
        length noRows `shouldEqual` 0
        liftEffect $ end pool

  describe "data types" do
    it "datetimes can be inserted" do
      pool <- liftEffect $ mkPool connectionInfo
      withClient pool \c -> do
        execute_ (Query "delete from types") c
        let date = canonicalDate <$> toEnum 2016 <*> Just January <*> toEnum 25
            time = Time <$> toEnum 23 <*> toEnum 1 <*> toEnum 59 <*> toEnum 0
            dt = DateTime <$> date <*> time
        maybe (fail "Not a datetime") (\ts -> do
          execute (Query "insert into types(timestamp_no_tz) VALUES ($1)") [toSql ts] c
          ts' <- queryValue_ read' (Query "select timestamp_no_tz at time zone 'UTC' from types" :: Query Foreign) c
          let res = unsafeCoerce <$> ts' >>= toDateTime
          res `shouldEqual` (Just ts)
        ) dt
        liftEffect $ end pool

  describe "sql arrays as parameters" $
    it "can be passed as a SqlValue" do
      pool <- liftEffect $ mkPool connectionInfo
      withClient pool \c -> do
        execute_ (Query "delete from artist") c
        execute_ (Query "insert into artist values ('Zed Leppelin', 1967)") c
        execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
        execute_ (Query "insert into artist values ('Deep Purple', 1969)") c
        artists <- query read' (Query "select * from artist where year = any ($1)" :: Query Artist) [toSql [1968, 1969]] c
        length artists `shouldEqual` 2
        liftEffect $ end pool

  describe "sql boolean as parameter" $
    it "can be passed as a SqlValue" do
      pool <- liftEffect $ mkPool connectionInfo
      withClient pool \c -> do
        execute_ (Query "delete from artist") c
        execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c -- false by default
        execute_ (Query "insert into artist values ('Deep Purple', 1969, TRUE)") c
        aliveArtists <- query read' (Query "select * from artist where isAlive = ($1)" :: Query Artist) [toSql true] c
        notAliveArtists <- query read' (Query "select * from artist where isAlive = ($1)" :: Query Artist) [toSql false] c
        length aliveArtists `shouldEqual` 1
        length notAliveArtists `shouldEqual` 1
        liftEffect $ end pool

  describe "transactions" do
    it "does not commit after an error inside a transation" do
      pool <- liftEffect $ mkPool connectionInfo
      withClient pool $ \c -> do
        execute_ (Query "delete from artist") c
        apathize $ tryInsert c
        one <- queryOne_ read' (Query "select * from artist" :: Query Artist) c

        one `shouldEqual` Nothing
        liftEffect $ end pool
          where
          tryInsert = withTransaction $ \c -> do
            execute_ (Query "insert into artist values ('Not there', 1999)") c
            throwError $ error "fail"

exampleError :: Aff (Maybe Artist)
exampleError = do
  pool <- liftEffect $ mkPool connectionInfo
  withClient pool $ \c -> do
    execute_ (Query "delete from artist") c
    execute_ (Query "insert into artist values ('Led Zeppelin', 1968)") c
    result <- queryOne_ read' (Query "select year from artist") c
    liftEffect $ end pool
    pure result

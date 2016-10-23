module Database.Postgres
  ( Query(..)
  , Client()
  , DB()
  , ConnectionInfo()
  , ConnectionString()
  , mkConnectionString
  , connect
  , disconnect
  , end
  , execute, execute_
  , query, query_
  , queryValue, queryValue_
  , queryOne, queryOne_
  , withConnection
  , withClient
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Either (Either, either)
import Data.Function.Uncurried (Fn2(), runFn2)
import Data.Array ((!!))
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign.Class (class IsForeign, read)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Control.Monad.Except (runExcept)
import Control.Monad.Aff (Aff, finally)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Traversable (sequence)

import Database.Postgres.SqlValue (SqlValue)

newtype Query a = Query String

foreign import data Client :: *

foreign import data DB :: !

type ConnectionString = String

type ConnectionInfo =
  { host :: String
  , db :: String
  , port :: Int
  , user :: String
  , password :: String
  }

mkConnectionString :: ConnectionInfo -> ConnectionString
mkConnectionString ci =
    "postgres://"
  <> ci.user <> ":"
  <> ci.password <> "@"
  <> ci.host <> ":"
  <> show ci.port <> "/"
  <> ci.db

-- | Makes a connection to the database.
connect :: forall eff. ConnectionInfo -> Aff (db :: DB | eff) Client
connect = connect' <<< mkConnectionString

-- | Runs a query and returns nothing.
execute :: forall eff a. Query a -> Array SqlValue -> Client -> Aff (db :: DB | eff) Unit
execute (Query sql) params client = void $ runQuery sql params client

-- | Runs a query and returns nothing
execute_ :: forall eff a. Query a -> Client -> Aff (db :: DB | eff) Unit
execute_ (Query sql) client = void $ runQuery_ sql client

-- | Runs a query and returns all results.
query :: forall eff a
  . (IsForeign a)
  => Query a -> Array SqlValue -> Client -> Aff (db :: DB | eff) (Array a)
query (Query sql) params client = do
  rows <- runQuery sql params client
  either liftError pure (runExcept (sequence $ read <$> rows))

-- | Just like `query` but does not make any param replacement
query_ :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Array a)
query_ (Query sql) client = do
  rows <- runQuery_ sql client
  either liftError pure (runExcept (sequence $ read <$> rows))

-- | Runs a query and returns the first row, if any
queryOne :: forall eff a
  . (IsForeign a)
  => Query a -> Array SqlValue -> Client -> Aff (db :: DB | eff) (Maybe a)
queryOne (Query sql) params client = do
  rows <- runQuery sql params client
  maybe (pure Nothing) (either liftError (pure <<< Just)) (readFirst rows)

-- | Just like `queryOne` but does not make any param replacement
queryOne_ :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
queryOne_ (Query sql) client = do
  rows <- runQuery_ sql client
  maybe (pure Nothing) (either liftError (pure <<< Just)) (readFirst rows)

-- | Runs a query and returns a single value, if any.
queryValue :: forall eff a
  . (IsForeign a)
  => Query a -> Array SqlValue -> Client -> Aff (db :: DB | eff) (Maybe a)
queryValue (Query sql) params client = do
  val <- runQueryValue sql params client
  pure $ either (const Nothing) Just (runExcept (read val))

-- | Just like `queryValue` but does not make any param replacement
queryValue_ :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
queryValue_ (Query sql) client = do
  val <- runQueryValue_ sql client
  either liftError (pure <<< Just) $ runExcept (read val)

-- | Connects to the database, calls the provided function with the client
-- | and returns the results.
withConnection :: forall eff a
  . ConnectionInfo
  -> (Client -> Aff (db :: DB | eff) a)
  -> Aff (db :: DB | eff) a
withConnection info p = do
  client <- connect info
  finally (p client) $ liftEff (end client)

-- | Takes a Client from the connection pool, runs the given function with
-- | the client and returns the results.
withClient :: forall eff a
  . ConnectionInfo
  -> (Client -> Aff (db :: DB | eff) a)
  -> Aff (db :: DB | eff) a
withClient info p = runFn2 _withClient (mkConnectionString info) p

readFirst :: forall a. IsForeign a => Array Foreign -> Maybe (Either MultipleErrors a)
readFirst rows = runExcept <<< read <$> (rows !! 0)

liftError :: forall e a. MultipleErrors -> Aff e a
liftError errs = throwError $ error (show errs)

foreign import connect' :: forall eff. String -> Aff (db :: DB | eff) Client

foreign import _withClient :: forall eff a. Fn2 ConnectionString (Client -> Aff (db :: DB | eff) a) (Aff (db :: DB | eff) a)

foreign import runQuery_ :: forall eff. String -> Client -> Aff (db :: DB | eff) (Array Foreign)

foreign import runQuery :: forall eff. String -> Array SqlValue -> Client -> Aff (db :: DB | eff) (Array Foreign)

foreign import runQueryValue_ :: forall eff. String -> Client -> Aff (db :: DB | eff) Foreign

foreign import runQueryValue :: forall eff. String -> Array SqlValue -> Client -> Aff (db :: DB | eff) Foreign

foreign import end :: forall eff. Client -> Eff (db :: DB | eff) Unit

foreign import disconnect :: forall eff. Eff (db :: DB | eff) Unit

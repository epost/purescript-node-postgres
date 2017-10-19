module Database.Postgres
  ( Query(..)
  , Client
  , Pool
  , DB
  , ConnectionInfo
  , ClientConfig
  , PoolConfig
  , ConnectionString
  , connectionInfoFromConfig
  , connectionInfoFromString
  , defaultPoolConfig
  , connect
  , release
  , end
  , execute, execute_
  , query, query_
  , queryValue, queryValue_
  , queryOne, queryOne_
  , withClient
  , mkPool
  ) where

import Prelude

import Control.Monad.Aff (Aff, bracket)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Array ((!!))
import Data.Either (Either, either)
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign.Class (class Decode, decode)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (sequence)
import Database.Postgres.SqlValue (SqlValue)
import Unsafe.Coerce (unsafeCoerce)

newtype Query a = Query String

foreign import data Pool :: Type

foreign import data Client :: Type

foreign import data DB :: Effect

foreign import data ConnectionInfo :: Type

type ConnectionString = String

connectionInfoFromString :: ConnectionString -> ConnectionInfo
connectionInfoFromString s = unsafeCoerce { connectionString: s }

type ClientConfig =
  { host :: String
  , database :: String
  , port :: Int
  , user :: String
  , password :: String
  , ssl :: Boolean
  }

type PoolConfig =
  { connectionTimeoutMillis :: Int
  , idleTimeoutMillis :: Int
  , max :: Int
  }

defaultPoolConfig :: PoolConfig
defaultPoolConfig =
  { connectionTimeoutMillis: 0
  , idleTimeoutMillis: 30000
  , max: 10
  }

connectionInfoFromConfig :: ClientConfig -> PoolConfig -> ConnectionInfo
connectionInfoFromConfig c p = unsafeCoerce
  { host: c.host
  , database: c.database
  , port: c.port
  , user: c.user
  , password: c.password
  , ssl: c.ssl
  , connectionTimeoutMillis: p.connectionTimeoutMillis
  , idleTimeoutMillis: p.idleTimeoutMillis
  , max: p.max
  }

-- | Makes a connection to the database via a Client.
connect :: forall eff. Pool -> Aff (db :: DB | eff) Client
connect = fromEffFnAff <<< connect'

-- | Runs a query and returns nothing.
execute :: forall eff a. Query a -> Array SqlValue -> Client -> Aff (db :: DB | eff) Unit
execute (Query sql) params client = void $ fromEffFnAff $ runQuery sql params client

-- | Runs a query and returns nothing
execute_ :: forall eff a. Query a -> Client -> Aff (db :: DB | eff) Unit
execute_ (Query sql) client = void $ fromEffFnAff $ runQuery_ sql client

-- | Runs a query and returns all results.
query :: forall eff a
  . Decode a
  => Query a -> Array SqlValue -> Client -> Aff (db :: DB | eff) (Array a)
query (Query sql) params client = do
  rows <- fromEffFnAff $ runQuery sql params client
  either liftError pure (runExcept (sequence $ decode <$> rows))

-- | Just like `query` but does not make any param replacement
query_ :: forall eff a
  . Decode a
  => Query a -> Client -> Aff (db :: DB | eff) (Array a)
query_ (Query sql) client = do
  rows <- fromEffFnAff $ runQuery_ sql client
  either liftError pure (runExcept (sequence $ decode <$> rows))

-- | Runs a query and returns the first row, if any
queryOne :: forall eff a
  . Decode a
  => Query a -> Array SqlValue -> Client -> Aff (db :: DB | eff) (Maybe a)
queryOne (Query sql) params client = do
  rows <- fromEffFnAff $ runQuery sql params client
  maybe (pure Nothing) (either liftError (pure <<< Just)) (decodeFirst rows)

-- | Just like `queryOne` but does not make any param replacement
queryOne_ :: forall eff a
  . Decode a
  => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
queryOne_ (Query sql) client = do
  rows <- fromEffFnAff $ runQuery_ sql client
  maybe (pure Nothing) (either liftError (pure <<< Just)) (decodeFirst rows)

-- | Runs a query and returns a single value, if any.
queryValue :: forall eff a
  . Decode a
  => Query a -> Array SqlValue -> Client -> Aff (db :: DB | eff) (Maybe a)
queryValue (Query sql) params client = do
  val <- fromEffFnAff $ runQueryValue sql params client
  pure $ either (const Nothing) Just (runExcept (decode val))

-- | Just like `queryValue` but does not make any param replacement
queryValue_ :: forall eff a
  . Decode a
  => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
queryValue_ (Query sql) client = do
  val <- fromEffFnAff $ runQueryValue_ sql client
  either liftError (pure <<< Just) $ runExcept (decode val)

-- | Connects to the database, calls the provided function with the client
-- | and returns the results.
withClient :: forall eff a
  . Pool -> (Client -> Aff (db :: DB | eff) a) -> Aff (db :: DB | eff) a
withClient pool p =
  bracket
    (connect pool)
    (liftEff <<< release)
    p

decodeFirst :: forall a. Decode a => Array Foreign -> Maybe (Either MultipleErrors a)
decodeFirst rows = runExcept <<< decode <$> (rows !! 0)

liftError :: forall e a. MultipleErrors -> Aff e a
liftError errs = throwError $ error (show errs)

foreign import mkPool :: forall eff. ConnectionInfo -> Eff (db :: DB | eff) Pool

foreign import connect' :: forall eff. Pool -> EffFnAff (db :: DB | eff) Client

foreign import runQuery_ :: forall eff. String -> Client -> EffFnAff (db :: DB | eff) (Array Foreign)

foreign import runQuery :: forall eff. String -> Array SqlValue -> Client -> EffFnAff (db :: DB | eff) (Array Foreign)

foreign import runQueryValue_ :: forall eff. String -> Client -> EffFnAff (db :: DB | eff) Foreign

foreign import runQueryValue :: forall eff. String -> Array SqlValue -> Client -> EffFnAff (db :: DB | eff) Foreign

foreign import release :: forall eff. Client -> Eff (db :: DB | eff) Unit

foreign import end :: forall eff. Pool -> Eff (db :: DB | eff) Unit

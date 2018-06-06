module Database.Postgres
  ( Query(..)
  , Client
  , Pool
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

import Control.Monad.Error.Class (throwError)
import Data.Array ((!!))
import Data.Either (Either, either)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (sequence)
import Database.Postgres.SqlValue (SqlValue)
import Effect (Effect)
import Effect.Aff (Aff, Error, bracket)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

newtype Query a = Query String

foreign import data Pool :: Type

foreign import data Client :: Type

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
connect :: Pool -> Aff Client
connect = fromEffectFnAff <<< connect'

-- | Runs a query and returns nothing.
execute :: forall a. Query a -> Array SqlValue -> Client -> Aff Unit
execute (Query sql) params client = void $ fromEffectFnAff $ runQuery sql params client

-- | Runs a query and returns nothing
execute_ :: forall a. Query a -> Client -> Aff Unit
execute_ (Query sql) client = void $ fromEffectFnAff $ runQuery_ sql client

-- | Runs a query and returns all results.
query :: forall a
  . (Foreign -> Either Error a) -> Query a -> Array SqlValue -> Client -> Aff (Array a)
query decode (Query sql) params client = do
  rows <- fromEffectFnAff $ runQuery sql params client
  either throwError pure (sequence $ decode <$> rows)

-- | Just like `query` but does not make any param replacement
query_ :: forall a
  . (Foreign -> Either Error a) -> Query a -> Client -> Aff (Array a)
query_ decode (Query sql) client = do
  rows <- fromEffectFnAff $ runQuery_ sql client
  either throwError pure (sequence $ decode <$> rows)

-- | Runs a query and returns the first row, if any
queryOne :: forall a
  . (Foreign -> Either Error a) -> Query a -> Array SqlValue -> Client -> Aff (Maybe a)
queryOne decode (Query sql) params client = do
  rows <- fromEffectFnAff $ runQuery sql params client
  maybe (pure Nothing) (either throwError (pure <<< Just)) (decodeFirst decode rows)

-- | Just like `queryOne` but does not make any param replacement
queryOne_ :: forall a
  . (Foreign -> Either Error a) -> Query a -> Client -> Aff (Maybe a)
queryOne_ decode (Query sql) client = do
  rows <- fromEffectFnAff $ runQuery_ sql client
  maybe (pure Nothing) (either throwError (pure <<< Just)) (decodeFirst decode rows)

-- | Runs a query and returns a single value, if any.
queryValue :: forall a
  . (Foreign -> Either Error a) -> Query a -> Array SqlValue -> Client -> Aff (Maybe a)
queryValue decode (Query sql) params client = do
  val <- fromEffectFnAff $ runQueryValue sql params client
  pure $ either (const Nothing) Just (decode val)

-- | Just like `queryValue` but does not make any param replacement
queryValue_ :: forall a
  . (Foreign -> Either Error a) -> Query a -> Client -> Aff (Maybe a)
queryValue_ decode (Query sql) client = do
  val <- fromEffectFnAff $ runQueryValue_ sql client
  either throwError (pure <<< Just) $ (decode val)

-- | Connects to the database, calls the provided function with the client
-- | and returns the results.
withClient :: forall a
  . Pool -> (Client -> Aff a) -> Aff a
withClient pool p =
  bracket
    (connect pool)
    (liftEffect <<< release)
    p

decodeFirst :: forall a. (Foreign -> Either Error a) -> Array Foreign -> Maybe (Either Error a)
decodeFirst decode rows = decode <$> (rows !! 0)

foreign import mkPool :: ConnectionInfo -> Effect Pool

foreign import connect' :: Pool -> EffectFnAff Client

foreign import runQuery_ :: String -> Client -> EffectFnAff (Array Foreign)

foreign import runQuery :: String -> Array SqlValue -> Client -> EffectFnAff (Array Foreign)

foreign import runQueryValue_ :: String -> Client -> EffectFnAff Foreign

foreign import runQueryValue :: String -> Array SqlValue -> Client -> EffectFnAff Foreign

foreign import release :: Client -> Effect Unit

foreign import end :: Pool -> Effect Unit

module Database.Postgres
  ( Query(..)
  , Client()
  , DB()
  , ConnectionInfo()
  , connect
  , end
  , execute
  , query
  , queryOne
  , queryValue
  , withConnection
  ) where

import Control.Alt
import Control.Monad.Eff
import Control.Monad.Trans
import Data.Either
import Data.Array
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception(Error(), error)
import Control.Monad.Error.Class (throwError)
import Data.Traversable (sequence)

newtype Query a = Query String

foreign import data Client :: *

foreign import data DB :: !

type ConnectionInfo =
  { host :: String
  , db :: String
  , port :: Number
  , user :: String
  , password :: String
  }

-- | Makes a connection to the database.
connect :: forall eff. ConnectionInfo -> Aff (db :: DB | eff) Client
connect ci = connect'
  $ "postgres://"
  <> ci.user <> ":"
  <> ci.password <> "@"
  <> ci.host <> ":"
  <> show ci.port <> "/"
  <> ci.db

-- | Runs a query and returns nothing.
execute :: forall eff a. Query a -> Client -> Aff (db :: DB | eff) Unit
execute (Query sql) client = void $ runQuery sql client

-- | Runs a query and returns all results.
query :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) [a]
query (Query sql) client = do
  rows <- runQuery sql client
  either liftError pure (sequence $ read <$> rows)

-- | Runs a query and returns the first row, if any
queryOne :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
queryOne (Query sql) client = do
  rows <- runQuery sql client
  maybe (pure Nothing) (either liftError (pure <<< Just)) $ read <$> (rows !! 0)

-- | Runs a query and returns a single value, if any.
queryValue :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
queryValue (Query sql) client = do
  val <- runQueryValue sql client
  either liftError (pure <<< Just) $ read val

-- | Connects to the database, calls the provided function with the client
-- | and returns the results.
withConnection :: forall eff a
  . ConnectionInfo
  -> (Client -> Aff (db :: DB | eff) a)
  -> Aff (db :: DB | eff) a
withConnection info p = do
  client <- connect info
  finally (p client) $ liftEff (end client)

liftError :: forall e a. ForeignError -> Aff e a
liftError err = throwError $ error (show err)

finally :: forall eff a. Aff eff a -> Aff eff Unit -> Aff eff a
finally a sequel = do
  res <- attempt a
  sequel
  either throwError pure res

foreign import connect' """
  function connect$prime(conString) {
    return function(success, error) {
      var pg = require('pg');
      var client = new pg.Client(conString);
      client.connect(function(err) {
        if (err) {
          error(err);
        } else {
          success(client);
        }
      })
      return client;
    }
  }
  """ :: forall eff. String -> Aff (db :: DB | eff) Client

foreign import runQuery """
  function runQuery(queryStr) {
    return function(client) {
      return function(success, error) {
        client.query(queryStr, function(err, result) {
          if (err) {
            error(err);
          } else {
            success(result.rows);
          }
        })
      };
    };
  }
  """ :: forall eff. String -> Client -> Aff (db :: DB | eff) [Foreign]

foreign import runQueryValue """
  function runQueryValue(queryStr) {
    return function(client) {
      return function(success, error) {
        client.query(queryStr, function(err, result) {
          if (err) return error(err);
          success(result.rows.length > 0 ? result.rows[0][result.fields[0].name] : undefined);
        })
      };
    };
  }
  """ :: forall eff. String -> Client -> Aff (db :: DB | eff) Foreign

foreign import end """
  function end(client) {
    return function() {
      client.end();
    };
  }
  """ :: forall eff. Client -> Eff (db :: DB | eff) Unit

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

import Control.Alt
import Control.Monad.Eff
import Control.Monad.Trans
import Data.Either
import Data.Function (Fn2(), runFn2)
import Data.Array
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception(Error(), error)
import Control.Monad.Error.Class (throwError)
import Data.Traversable (sequence)

import Database.Postgres.SqlValue

newtype Query a = Query String

foreign import data Client :: *

foreign import data DB :: !

type ConnectionString = String

type ConnectionInfo =
  { host :: String
  , db :: String
  , port :: Number
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
execute :: forall eff a. Query a -> [SqlValue] -> Client -> Aff (db :: DB | eff) Unit
execute (Query sql) params client = void $ runQuery sql params client

-- | Runs a query and returns nothing
execute_ :: forall eff a. Query a -> Client -> Aff (db :: DB | eff) Unit
execute_ (Query sql) client = void $ runQuery_ sql client

-- | Runs a query and returns all results.
query :: forall eff a p
  . (IsForeign a)
  => Query a -> [SqlValue] -> Client -> Aff (db :: DB | eff) [a]
query (Query sql) params client = do
  rows <- runQuery sql params client
  either liftError pure (sequence $ read <$> rows)

-- | Just like `query` but does not make any param replacement
query_ :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) [a]
query_ (Query sql) client = do
  rows <- runQuery_ sql client
  either liftError pure (sequence $ read <$> rows)

-- | Runs a query and returns the first row, if any
queryOne :: forall eff a
  . (IsForeign a)
  => Query a -> [SqlValue] -> Client -> Aff (db :: DB | eff) (Maybe a)
queryOne (Query sql) params client = do
  rows <- runQuery sql params client
  maybe (pure Nothing) (either liftError (pure <<< Just)) $ read <$> (rows !! 0)

-- | Just like `queryOne` but does not make any param replacement
queryOne_ :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
queryOne_ (Query sql) client = do
  rows <- runQuery_ sql client
  maybe (pure Nothing) (either liftError (pure <<< Just)) $ read <$> (rows !! 0)

-- | Runs a query and returns a single value, if any.
queryValue :: forall eff a
  . (IsForeign a)
  => Query a -> [SqlValue] -> Client -> Aff (db :: DB | eff) (Maybe a)
queryValue (Query sql) params client = do
  val <- runQueryValue sql params client
  pure $ either (const Nothing) Just (read val)

-- | Just like `queryValue` but does not make any param replacement
queryValue_ :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
queryValue_ (Query sql) client = do
  val <- runQueryValue_ sql client
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

-- | Takes a Client from the connection pool, runs the given function with
-- | the client and returns the results.
withClient :: forall eff a
  . ConnectionInfo
  -> (Client -> Aff (db :: DB | eff) a)
  -> Aff (db :: DB | eff) a
withClient info p = runFn2 _withClient (mkConnectionString info) p

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

foreign import _withClient
  """
  function _withClient(conString, cb) {
    return function(success, error) {
      var pg = require('pg');
      pg.connect(conString, function(err, client, done) {
        if (err) {
          done(true);
          return error(err);
        }
        cb(client)(function(v) {
          done();
          success(v);
        }, function(err) {
          done();
          error(err);
        })
      });
    };
  }
  """ :: forall eff a.
      Fn2
      ConnectionString
      (Client -> Aff (db :: DB | eff) a)
      (Aff (db :: DB | eff) a)

foreign import runQuery_ """
  function runQuery_(queryStr) {
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

foreign import runQuery """
  function runQuery(queryStr) {
    return function(params) {
      return function(client) {
        return function(success, error) {
          client.query(queryStr, params, function(err, result) {
            if (err) return error(err);
            success(result.rows);
          })
        };
      };
    }
  }
  """ :: forall eff. String -> [SqlValue] -> Client -> Aff (db :: DB | eff) [Foreign]

foreign import runQueryValue_ """
  function runQueryValue_(queryStr) {
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

foreign import runQueryValue """
  function runQueryValue(queryStr) {
    return function(params) {
      return function(client) {
        return function(success, error) {
          client.query(queryStr, params, function(err, result) {
            if (err) return error(err);
            success(result.rows.length > 0 ? result.rows[0][result.fields[0].name] : undefined);
          })
        };
      };
    }
  }
  """ :: forall eff. String -> [SqlValue] -> Client -> Aff (db :: DB | eff) Foreign

foreign import end """
  function end(client) {
    return function() {
      client.end();
    };
  }
  """ :: forall eff. Client -> Eff (db :: DB | eff) Unit

foreign import disconnect
  """
  function disconnect() {
    var pg = require('pg');
    pg.end();
  }
  """ :: forall eff. Eff (db :: DB | eff) Unit

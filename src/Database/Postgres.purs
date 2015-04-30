module Database.Postgres
  ( Query(..)
  , Connection()
  , DB()
  , ConnectionInfo()
  , ConnectionString()
  , mkConnectionString
  , connect
  , end
  , execute, execute_
  , query, query_
  , queryValue, queryValue_
  , queryOne, queryOne_
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

import Database.Postgres.SqlValue

newtype Query a = Query String

foreign import data Connection :: *

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
connect :: forall eff. ConnectionInfo -> Aff (db :: DB | eff) Connection
connect = connect' <<< mkConnectionString

-- | Runs a query and returns nothing.
execute :: forall eff a. Query a -> [SqlValue] -> Connection -> Aff (db :: DB | eff) Unit
execute (Query sql) params con = void $ runQuery sql params con

-- | Runs a query and returns nothing
execute_ :: forall eff a. Query a -> Connection -> Aff (db :: DB | eff) Unit
execute_ (Query sql) con = void $ runQuery_ sql con

-- | Runs a query and returns all results.
query :: forall eff a
  . (IsForeign a)
  => Query a -> [SqlValue] -> Connection -> Aff (db :: DB | eff) [F a]
query (Query sql) params con = do
  rows <- runQuery sql params con
  pure $ read <$> rows

-- | Just like `query` but does not make any param replacement
query_ :: forall eff a. (IsForeign a) => Query a -> Connection -> Aff (db :: DB | eff) [a]
query_ (Query sql) con = do
  rows <- runQuery_ sql con
  either liftError pure (sequence $ read <$> rows)

-- | Runs a query and returns the first row, if any
queryOne :: forall eff a
  . (IsForeign a)
  => Query a -> [SqlValue] -> Connection -> Aff (db :: DB | eff) (Maybe a)
queryOne (Query sql) params con = do
  rows <- runQuery sql params con
  maybe (pure Nothing) (either liftError (pure <<< Just)) $ read <$> (rows !! 0)

-- | Just like `queryOne` but does not make any param replacement
queryOne_ :: forall eff a. (IsForeign a) => Query a -> Connection -> Aff (db :: DB | eff) (Maybe a)
queryOne_ (Query sql) con = do
  rows <- runQuery_ sql con
  maybe (pure Nothing) (either liftError (pure <<< Just)) $ read <$> (rows !! 0)

-- | Runs a query and returns a single value, if any.
queryValue :: forall eff a
  . (IsForeign a)
  => Query a -> [SqlValue] -> Connection -> Aff (db :: DB | eff) (Maybe a)
queryValue (Query sql) params con = do
  val <- runQueryValue sql params con
  pure $ either (const Nothing) Just (read val)

-- | Just like `queryValue` but does not make any param replacement
queryValue_ :: forall eff a. (IsForeign a) => Query a -> Connection -> Aff (db :: DB | eff) (Maybe a)
queryValue_ (Query sql) con = do
  val <- runQueryValue_ sql con
  either liftError (pure <<< Just) $ read val

-- | Connects to the database, calls the provided function with the connection
-- | and returns the results.
withConnection :: forall eff a
  . ConnectionInfo
  -> (Connection -> Aff (db :: DB | eff) a)
  -> Aff (db :: DB | eff) a
withConnection info p = do
  con <- connect info
  finally (p con) $ liftEff (end con)

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
      var anyDB = require('any-db');
      anyDB.createConnection(conString, function(err, con) {
        if (err) {
          error(err);
        } else {
          success(con);
        }
      });
    };
  }
  """ :: forall eff. ConnectionString -> Aff (db :: DB | eff) Connection

foreign import runQuery_ """
  function runQuery_(queryStr) {
    return function(con) {
      return function(success, error) {
        con.query(queryStr, function(err, result) {
          if (err) {
            error(err);
          } else {
            success(result.rows);
          }
        })
      };
    };
  }
  """ :: forall eff. String -> Connection -> Aff (db :: DB | eff) [Foreign]

foreign import runQuery """
  function runQuery(queryStr) {
    return function(params) {
      return function(con) {
        return function(success, error) {
          con.query(queryStr, params, function(err, result) {
            if (err) return error(err);
            success(result.rows);
          })
        };
      };
    }
  }
  """ :: forall eff. String -> [SqlValue] -> Connection -> Aff (db :: DB | eff) [Foreign]

foreign import runQueryValue_ """
  function runQueryValue_(queryStr) {
    return function(con) {
      return function(success, error) {
        con.query(queryStr, function(err, result) {
          if (err) return error(err);
          success(result.rows.length > 0 ? result.rows[0][result.fields[0].name] : undefined);
        })
      };
    };
  }
  """ :: forall eff. String -> Connection -> Aff (db :: DB | eff) Foreign

foreign import runQueryValue """
  function runQueryValue(queryStr) {
    return function(params) {
      return function(con) {
        return function(success, error) {
          con.query(queryStr, params, function(err, result) {
            if (err) return error(err);
            success(result.rows.length > 0 ? result.rows[0][result.fields[0].name] : undefined);
          })
        };
      };
    }
  }
  """ :: forall eff. String -> [SqlValue] -> Connection -> Aff (db :: DB | eff) Foreign

foreign import end """
  function end(con) {
    return function() {
      con.end();
    };
  }
  """ :: forall eff. Connection -> Eff (db :: DB | eff) Unit

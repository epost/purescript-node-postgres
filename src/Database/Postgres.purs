module Database.Postgres where

import Control.Monad.Eff
import Data.Either
import Data.Array
import Data.Foreign
import Data.Foreign.Class

type Query = String

foreign import data Client :: *

foreign import data DB :: !

type DBEff a = forall eff. Eff (db :: DB | eff) a

type ConnectionInfo =
  { host :: String
  , db :: String
  , port :: Number
  , user :: String
  , password :: String
  }

connect :: ConnectionInfo -> DBEff Client
connect ci = connectJS $ "postgres://" <> ci.user <> ":" <> ci.password <> "@" <> ci.host <> ":" <> show ci.port <> "/" <> ci.db

foreign import connectJS """
  function connectJS(conString) {
    return function() {
      var pg = require('pg');
      var client = new pg.Client(conString);
      return client;
    };
  }
""" :: String -> DBEff Client


foreign import connectClient """
  function connectClient(client) {
    return function() {
      client.connect();
    };
  }
""" :: Client -> DBEff Unit

foreign import endClient """
  function endClient(client) {
    return function() {
      client.end();
    };
  }
""" :: Client -> DBEff Unit


foreign import runQueryRowsForeignRaw """
  function runQueryRowsForeignRaw(client) {
      return function (queryStr) {
        return function(onRow) {
            return function() {
              client.query(queryStr, function(err, result) {
                if (err) {
                  console.error('error running query', err);
                } else {
                  onRow(result.rows)();
                }
              });
            };
          };
        };
  }
  """ :: forall eff. Client -> Query -> ([Foreign] -> eff) -> DBEff Unit


runQuery :: forall row eff. (IsForeign row) =>
            Client -> Query -> ([F row] -> eff) -> DBEff Unit
runQuery client query handleRows = runQueryRowsForeignRaw client query (map read >>> handleRows)

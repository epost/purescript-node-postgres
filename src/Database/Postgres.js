
// module Database.Postgres

import pg from 'pg';

export function mkPool(conInfo) {
  return function () {
    return new pg.Pool(conInfo);
  };
}

export function connectImpl(pool) {
  return function(error, success) {
    pool.connect(function(err, client) {
      if (err) {
        error(err);
      } else {
        success(client);
      }
    });
    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
  };
}

export function runQuery_(queryStr) {
  return function(client) {
    return function(error, success) {
      client.query(queryStr, function(err, result) {
        if (err) {
          error(err);
        } else {
          success(result.rows);
        }
      });
      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
      };
    };
  };
}

export function runQuery(queryStr) {
  return function(params) {
    return function(client) {
      return function(error, success) {
        client.query(queryStr, params, function(err, result) {
          if (err) return error(err);
          success(result.rows);
        });
        return function(cancelError, onCancelerError, onCancelerSuccess) {
          onCancelerSuccess();
        };
      };
    };
  };
}

export function runQueryValue_(queryStr) {
  return function(client) {
    return function(error, success) {
      client.query(queryStr, function(err, result) {
        if (err) return error(err);
        success(result.rows.length > 0 ? result.rows[0][result.fields[0].name] : undefined);
      });
      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
      };
    };
  };
}

export function runQueryValue(queryStr) {
  return function(params) {
    return function(client) {
      return function(error, success) {
        client.query(queryStr, params, function(err, result) {
          if (err) return error(err);
          success(result.rows.length > 0 ? result.rows[0][result.fields[0].name] : undefined);
        });
        return function(cancelError, onCancelerError, onCancelerSuccess) {
          onCancelerSuccess();
        };
      };
    };
  };
}

export function release(client) {
  return function () {
    client.release();
  };
}

export function end(pool) {
  return function() {
    pool.end();
  };
}

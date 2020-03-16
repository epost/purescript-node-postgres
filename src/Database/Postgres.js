
// module Database.Postgres

var pg = require('pg');

exports.mkPool = function (conInfo) {
  return function () {
    return new pg.Pool(conInfo);
  };
}

exports.connectImpl = function (pool) {
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

exports.runQuery_ = function(queryStr) {
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

exports.runQuery = function(queryStr) {
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

exports.runQueryValue_ = function(queryStr) {
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

exports.runQueryValue = function(queryStr) {
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

exports.release = function (client) {
  return function () {
    client.release();
  };
}

exports.end = function(pool) {
  return function() {
    pool.end();
  };
}

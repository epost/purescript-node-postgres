
// module Database.Postgres

var pg = require('pg');

exports["connect'"] = function (conString) {
  return function(success, error) {
    var client = new pg.Client(conString);
    client.connect(function(err) {
      if (err) {
        error(err);
      } else {
        success(client);
      }
    })
    return client;
  };
}

exports._withClient = function (conString, cb) {
  return function(success, error) {
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

exports.runQuery_ = function (queryStr) {
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

exports.runQuery = function (queryStr) {
  return function(params) {
    return function(client) {
      return function(success, error) {
        client.query(queryStr, params, function(err, result) {
          if (err) return error(err);
          success(result.rows);
        })
      };
    };
  };
}

exports.runQueryValue_ = function (queryStr) {
  return function(client) {
    return function(success, error) {
      client.query(queryStr, function(err, result) {
        if (err) return error(err);
        success(result.rows.length > 0 ? result.rows[0][result.fields[0].name] : undefined);
      })
    };
  };
}

exports.runQueryValue = function (queryStr) {
  return function(params) {
    return function(client) {
      return function(success, error) {
        client.query(queryStr, params, function(err, result) {
          if (err) return error(err);
          success(result.rows.length > 0 ? result.rows[0][result.fields[0].name] : undefined);
        })
      };
    };
  };
}

exports.end = function (client) {
  return function() {
    client.end();
  };
}

exports.disconnect = function () {
  pg.end();
}

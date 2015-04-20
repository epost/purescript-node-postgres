# Module Documentation

## Module Database.Postgres

#### `Query`

``` purescript
newtype Query a
  = Query String
```


#### `Client`

``` purescript
data Client :: *
```


#### `DB`

``` purescript
data DB :: !
```


#### `ConnectionInfo`

``` purescript
type ConnectionInfo = { password :: String, user :: String, port :: Number, db :: String, host :: String }
```


#### `connect`

``` purescript
connect :: forall eff. ConnectionInfo -> Aff (db :: DB | eff) Client
```

Makes a connection to the database

#### `execute`

``` purescript
execute :: forall eff a. Query a -> Client -> Aff (db :: DB | eff) Unit
```

Runs a query and returns nothing

#### `query`

``` purescript
query :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) [a]
```

Runs a query and returns all results

#### `queryOne`

``` purescript
queryOne :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
```

Runs a query and returns the first row, if any

#### `queryValue`

``` purescript
queryValue :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
```

Runs a query and returns a single value, if any

#### `withConnection`

``` purescript
withConnection :: forall eff a. ConnectionInfo -> (Client -> Aff (db :: DB | eff) a) -> Aff (db :: DB | eff) a
```

Connects to the database, calls the provided function with the client
and returns the results.

#### `end`

``` purescript
end :: forall eff. Client -> Eff (db :: DB | eff) Unit
```





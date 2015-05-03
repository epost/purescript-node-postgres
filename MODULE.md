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


#### `ConnectionString`

``` purescript
type ConnectionString = String
```


#### `ConnectionInfo`

``` purescript
type ConnectionInfo = { password :: String, user :: String, port :: Number, db :: String, host :: String }
```


#### `mkConnectionString`

``` purescript
mkConnectionString :: ConnectionInfo -> ConnectionString
```


#### `connect`

``` purescript
connect :: forall eff. ConnectionInfo -> Aff (db :: DB | eff) Client
```

Makes a connection to the database.

#### `execute`

``` purescript
execute :: forall eff a. Query a -> [SqlValue] -> Client -> Aff (db :: DB | eff) Unit
```

Runs a query and returns nothing.

#### `execute_`

``` purescript
execute_ :: forall eff a. Query a -> Client -> Aff (db :: DB | eff) Unit
```

Runs a query and returns nothing

#### `query`

``` purescript
query :: forall eff a p. (IsForeign a) => Query a -> [SqlValue] -> Client -> Aff (db :: DB | eff) [a]
```

Runs a query and returns all results.

#### `query_`

``` purescript
query_ :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) [a]
```

Just like `query` but does not make any param replacement

#### `queryOne`

``` purescript
queryOne :: forall eff a. (IsForeign a) => Query a -> [SqlValue] -> Client -> Aff (db :: DB | eff) (Maybe a)
```

Runs a query and returns the first row, if any

#### `queryOne_`

``` purescript
queryOne_ :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
```

Just like `queryOne` but does not make any param replacement

#### `queryValue`

``` purescript
queryValue :: forall eff a. (IsForeign a) => Query a -> [SqlValue] -> Client -> Aff (db :: DB | eff) (Maybe a)
```

Runs a query and returns a single value, if any.

#### `queryValue_`

``` purescript
queryValue_ :: forall eff a. (IsForeign a) => Query a -> Client -> Aff (db :: DB | eff) (Maybe a)
```

Just like `queryValue` but does not make any param replacement

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



## Module Database.Postgres.SqlValue

#### `SqlValue`

``` purescript
data SqlValue :: *
```


#### `IsSqlValue`

``` purescript
class IsSqlValue a where
  toSql :: a -> SqlValue
```


#### `isSqlValueString`

``` purescript
instance isSqlValueString :: IsSqlValue String
```


#### `isSqlValueNumber`

``` purescript
instance isSqlValueNumber :: IsSqlValue Number
```


#### `isSqlValueInt`

``` purescript
instance isSqlValueInt :: IsSqlValue Int
```


#### `isSqlValueMaybe`

``` purescript
instance isSqlValueMaybe :: (IsSqlValue a) => IsSqlValue (Maybe a)
```





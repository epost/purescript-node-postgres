module Database.Postgres.SqlValue
  ( SqlValue()
  , class IsSqlValue
  , toSql
  ) where

import Prelude ((<<<))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Date as Date

foreign import data SqlValue :: *

class IsSqlValue a where
  toSql :: a -> SqlValue

instance isSqlValueString :: IsSqlValue String where
  toSql = unsafeToSqlValue

instance isSqlValueNumber :: IsSqlValue Number where
  toSql = unsafeToSqlValue

instance isSqlValueInt :: IsSqlValue Int where
  toSql = unsafeToSqlValue <<< toNumber

instance isSqlValueMaybe :: (IsSqlValue a) => IsSqlValue (Maybe a) where
  toSql Nothing = nullSqlValue
  toSql (Just x) = toSql x

instance isSqlValueDate :: IsSqlValue Date.Date where
  toSql = unsafeToSqlValue

foreign import unsafeToSqlValue :: forall a. a -> SqlValue

foreign import nullSqlValue :: SqlValue

module Database.Postgres.SqlValue
  ( SqlValue()
  , class IsSqlValue
  , toSql
  ) where

import Prelude
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Date (year, month, day)
import Data.DateTime (DateTime(DateTime))
import Data.Time (hour, minute, second)

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

instance isSqlValueDateTime :: IsSqlValue DateTime where
  toSql = toSql <<< format
    where
      format (DateTime d t)
        = show (fromEnum (year d)) <> "-"
        <> zeroPad (fromEnum (month d)) <> "-"
        <> zeroPad (fromEnum (day d)) <> " "
        <> zeroPad (fromEnum (hour t)) <> ":"
        <> zeroPad (fromEnum (minute t)) <> ":"
        <> zeroPad (fromEnum (second t))

      zeroPad :: Int -> String
      zeroPad i | i < 10 = "0" <> (show i)
      zeroPad i = show i

foreign import unsafeToSqlValue :: forall a. a -> SqlValue

foreign import nullSqlValue :: SqlValue

module Database.Postgres.SqlValue
  ( SqlValue()
  , class IsSqlValue
  , toSql
  ) where

import Prelude
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Date (year, month, day)
import Data.DateTime (DateTime(DateTime))
import Data.Time (hour, minute, second)
import Unsafe.Coerce (unsafeCoerce)
import Data.Nullable (toNullable)

foreign import data SqlValue :: Type

class IsSqlValue a where
  toSql :: a -> SqlValue

instance isSqlValueString :: IsSqlValue String where
  toSql = unsafeCoerce

instance isSqlValueBoolean :: IsSqlValue Boolean where
  toSql val =
    if val
      then toSql "true"
      else toSql "false"

instance isSqlValueNumber :: IsSqlValue Number where
  toSql = unsafeCoerce

instance isSqlValueInt :: IsSqlValue Int where
  toSql = unsafeCoerce <<< toNumber

instance isSqlValueMaybe :: (IsSqlValue a) => IsSqlValue (Maybe a) where
  toSql = unsafeCoerce <<< toNullable <<< (toSql <$> _)

instance isSqlValueArray :: (IsSqlValue a) => IsSqlValue (Array a) where
  toSql = unsafeCoerce <<< map toSql

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

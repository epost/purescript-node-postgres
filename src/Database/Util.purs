module Util where

import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Index (prop)
import Control.Bind

--------------------------------------------------------------------------------

-- | Given a foreign object like { foo: 123 } having exactly one property, read that property's value (123).
readSingularProperty :: forall a. (IsForeign a) => Foreign -> Either ForeignError a
readSingularProperty obj = read =<< singularPropertyValueForeign obj

singularPropertyValueForeign :: Foreign -> Either ForeignError Foreign
singularPropertyValueForeign obj = do
  key <- singularPropertyKey obj
  prop key obj

-- naming: property vs field vs key vs ...
singularPropertyKey :: Foreign -> Either ForeignError String
singularPropertyKey obj = case objectKeys obj of
  [fieldName] -> Right fieldName
  []          -> Left $ TypeMismatch "object with a single property" "non-object or an object without properties"
  other       -> Left $ TypeMismatch "object with a single property" "object with more than one property"

-- TODO Object.keys is ECMAScript 5, but that shouldn't be a problem for Node.js
foreign import objectKeys """
  function objectKeys(obj) {
      try {
        return Object.keys(obj);
      } catch (err) {
        return [];
      }
  }
""" :: Foreign -> [String]

--------------------------------------------------------------------------------

foreign import showUnsafe """
  function showUnsafe(x) { console.log('showUnsafe: ', x); return x.toString(); };
""" :: forall a. a -> String

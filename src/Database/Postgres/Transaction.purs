module Database.Postgres.Transaction where

import Prelude
import Effect.Aff (Aff, attempt)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)

import Database.Postgres (Client, Query(Query), execute_)

-- | Runs an asynchronous action in a database transaction. The transaction
-- | will be rolled back if the computation fails and committed otherwise.
-- |
-- | Here the first insert will be rolled back:
-- |
-- | ```purescript
-- | moneyTransfer :: forall e. (Client -> Aff e Unit) -> Client -> Aff e Unit
-- | moneyTransfer = withTransaction $ \c -> do
-- |   execute_ (Query "insert into accounts ...") c
-- |   throwError $ error "Something went wrong"
-- |   execute_ (Query "insert into accounts ...") c
-- | ```
withTransaction :: forall a. (Client -> Aff a) -> Client -> Aff a
withTransaction act client = do
  begin client
  res <- attempt (act client)
  either rollback_ commit_ res
    where
    rollback_ err = rollback client *> throwError err
    commit_ v = commit client *> pure v

begin :: Client -> Aff Unit
begin = execute_ (Query "BEGIN TRANSACTION")

commit :: Client -> Aff Unit
commit = execute_ (Query "COMMIT")

rollback :: Client -> Aff Unit
rollback = execute_ (Query "ROLLBACK")

module Database.Postgres.Transaction where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Aff
import Control.Monad.Error.Class (throwError)
import Data.Either

import Database.Postgres
import Database.Postgres.SqlValue

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
withTransaction :: forall eff a. (Client -> Aff (db :: DB | eff) a) -> Client -> Aff (db :: DB | eff) a
withTransaction act client = do
  begin client
  res <- attempt (act client)
  either rollback_ commit_ res
    where
    rollback_ err = rollback client *> throwError err
    commit_ v = commit client *> pure v

begin :: forall eff. Client -> Aff (db :: DB | eff) Unit
begin = execute_ (Query "BEGIN TRANSACTION")

commit :: forall eff. Client -> Aff (db :: DB | eff) Unit
commit = execute_ (Query "COMMIT")

rollback :: forall eff. Client -> Aff (db :: DB | eff) Unit
rollback = execute_ (Query "ROLLBACK")

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | A thin MySQL effect.
--
-- See the documentation of 'mysql-simple' for details regarding the
-- various functions.
module Control.Eff.MySQL
  ( query
  , query_
  -- , fold
  -- , fold_
  -- , forEach
  -- , forEach_
  , execute
  , execute_
  , executeMany
  , insertID
  -- , withTransaction
  , autocommit
  , commit
  , rollback
  , formatMany
  , formatQuery
  , runMySQL
  , runMySQLWithConnection
  -- | reexports from mysql-simple
  , M.ConnectInfo(..)
  , M.In(..)
  , M.Only(..)
  , M.Query
  , M.QueryParams(..)
  , M.QueryResults(..)
  , M.defaultConnectInfo
  ,
  ) where

import Control.Eff
import Control.Eff.Lift
import Control.Eff.MySQL.Helper
import Control.Eff.Reader.Strict
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Word (Word64)
import qualified Database.MySQL.Simple as M
import qualified Database.MySQL.Simple.QueryParams as M
import qualified Database.MySQL.Simple.QueryResults as M

type MySQL = Reader M.Connection

-- | Run the MySQL effect. In case of exceptions it will not close the
-- connection. (That will still be done by the GC at one point.)
runMySQL
  :: (Member (Lift IO) r)
  => Eff (MySQL :> r) a -> M.ConnectInfo -> Eff r a
runMySQL e c = do
    conn <- lift $ M.connect c
    let res = runMySQLWithConnection e conn
    lift $ M.close conn
    res

-- | Run the MySQL effect with a given 'M.Connection'.
runMySQLWithConnection :: Eff (MySQL :> r) a -> M.Connection -> Eff r a
runMySQLWithConnection = runReader

-- | See 'M.query' for details.
query
  :: ( Member (Lift IO) r, Member MySQL r
    , M.QueryResults a, M.QueryParams p)
  => M.Query -> p -> Eff r [a]
query = askLift2 M.query

-- | See 'M.query_' for details.
query_
  :: (Member (Lift IO) r, Member MySQL r, M.QueryResults a)
  => M.Query -> Eff r [a]
query_ = askLift M.query_

-- -- | See 'M.fold' for details.
-- fold
--   :: (Member (Lift IO) r, Member MySQL r, M.QueryResults b, M.QueryParams p)
--   => M.Query -> p -> a -> (a -> b -> Eff r a) -> Eff r [a]
-- -- | See 'M.fold_' for details.
-- fold_
--   :: (Member (Lift IO) r, Member MySQL r, M.QueryResults b)
--   => M.Query -> q -> a -> (a -> b -> Eff r a) -> Eff r [a]
-- -- | See 'M.forEach' for details.
-- forEach
--   :: (Member (Lift IO) r, Member MySQL r, M.QueryResults b, M.QueryParams p)
--   => M.Query -> p -> (b -> Eff r ()) -> Eff r ()
-- -- | See 'M.forEach_' for details.
-- forEach_
--   :: (Member (Lift IO) r, Member MySQL r, M.QueryResults b)
--   => M.Query -> (b -> Eff r ()) -> Eff r ()

-- | See 'M.execute' for details.
execute
  :: (Member (Lift IO) r, Member MySQL r, M.QueryParams p)
  => M.Query -> p -> Eff r Int64
execute = askLift2 M.execute

-- | See 'M.execute_' for details.
execute_
  :: (Member (Lift IO) r, Member MySQL r)
  => M.Query -> Eff r Int64
execute_ = askLift M.execute_

-- | See 'M.executeMany' for details.
executeMany
  :: (Member (Lift IO) r, Member MySQL r, M.QueryParams p)
  => M.Query -> [p] -> Eff r Int64
executeMany = askLift2 M.executeMany

-- | See 'M.insertID ' for details.
insertID :: (Member (Lift IO) r, Member MySQL r) => Eff r Word64
insertID = askLift0 M.insertID

-- -- | See 'M.withTransaction' for details.
-- withTransaction
--   :: (Member (Lift IO) r, Member MySQL r)
--   => Eff r a -> Eff r a

-- | See 'M.autocommit ' for details.
autocommit :: (Member (Lift IO ) r, Member MySQL r) => Bool -> Eff r ()
autocommit = askLift M.autocommit

-- | See 'M.commit ' for details.
commit :: (Member (Lift IO ) r, Member MySQL r) => Eff r ()
commit = askLift0 M.commit

-- | See 'M.rollback ' for details.
rollback :: (Member (Lift IO ) r, Member MySQL r) => Eff r ()
rollback = askLift0 M.rollback

-- | See 'M.formatMany' for details.
formatMany
  :: (Member (Lift IO) r, Member MySQL r, M.QueryParams p)
  => M.Query -> [p] -> Eff r ByteString
formatMany = askLift2 M.formatMany

-- | See 'M.formatQuery' for details.
formatQuery
  :: (Member (Lift IO) r, Member MySQL r, M.QueryParams p)
  => M.Query -> p -> Eff r ByteString
formatQuery = askLift2 M.formatQuery

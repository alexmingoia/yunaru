{-# LANGUAGE OverloadedStrings #-}

module App.Model.Database
  ( module App.Model.Database,
    module Database.Selda,
    module Database.Selda.SqlType,
    module Database.Selda.PostgreSQL,
    module Database.Selda.Backend,
    module Control.Monad.IO.Class,
    module App.Model.Selda,
  )
where

import App.Model.Env
import App.Model.Selda
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Pool
import Database.Selda hiding (def)
import Database.Selda.Backend
import Database.Selda.PostgreSQL
import Database.Selda.SqlType
import Network.Wai.Responder

openConn :: AppEnv -> IO AppDBConn
openConn e = pgOpen (appDbName e `on` appDbHost e `auth` (appDbUser e, appDbPass e))

closeConn :: AppDBConn -> IO ()
closeConn = seldaClose

-- | Run IO action with database connected environment.
withConn :: (AppEnv -> IO a) -> IO a
withConn act = do
  env <- getAppEnv
  bracket (openConn env) closeConn $ \conn -> act (env {appDbConn = Just (Left conn)})

-- | Initialize connection pool for environment.
withPool :: AppEnv -> IO AppEnv
withPool env = do
  pool <- createPool (openConn env) closeConn 2 60 10
  return (env {appDbConn = Just (Right pool)})

-- | Execute query within Responder monadic context, responding with errors if encountered.
exec :: SeldaT PG IO a -> Responder AppEnv IO a
exec q = do
  env <- getEnv
  liftIO (execEnv env q)

-- | Execute query with a given database connection.
execEnv :: AppEnv -> SeldaT PG IO a -> IO a
execEnv env q =
  case appDbConn env of
    Nothing -> do
      when (appDebug env) (putStrLn "No database connection.")
      throwIO (InternalError "We are taking a short nap. Check back later.")
    Just (Left conn) -> runSeldaT q conn
    Just (Right pool) -> withResource pool (runSeldaT q)

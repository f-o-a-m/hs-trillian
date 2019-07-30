module Trillian.Examples.ConfigUtils where

import           Control.Error          (ExceptT, runExceptT, (!?), (??))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Char              (toLower)
import           Data.Maybe             (isJust)
import           System.Environment     (lookupEnv)
import qualified Text.Read              as T

readEnvVar :: (Read a, MonadIO m) => String -> ExceptT String m a
readEnvVar var = do
  str <- liftIO (lookupEnv var) !? ("Missing Environment Variable: " ++ var)
  T.readMaybe str ?? ("Couldn't Parse Environment Variable " ++ var ++ ": " ++ str)

readEnvVarWithDefault :: (Read a, Show a, MonadIO m) => String -> a -> ExceptT String m a
readEnvVarWithDefault var def = liftIO (lookupEnv var) >>= \case
    Nothing -> do
      liftIO $ putStrLn $ "Defaulting " <> show var <> " to " <> show def <> " as it was not set in the environment"
      return def
    Just s -> T.readMaybe s ?? ("Couldn't Parse Environment Variable " ++ var ++ ": " ++ s)

getEnvVar :: MonadIO m => String -> ExceptT String m String
getEnvVar var = liftIO (lookupEnv var) !? ("Missing Environment Variable: " ++ var)

getEnvVarWithDefault :: MonadIO m => String -> String -> ExceptT String m String
getEnvVarWithDefault var def = liftIO (lookupEnv var) >>= \case
  Nothing -> do
    liftIO $ putStrLn $ "Defaulting " <> show var <> " to " <> show def <> " as it was not set in the environment"
    return def
  Just x -> return x

getEnvVarBool :: MonadIO m => String -> ExceptT String m Bool
getEnvVarBool var = do
  val <- liftIO (lookupEnv var) !? ("Missing Environment Variable: " ++ var)
  let normalized = toLower <$> val
  if normalized `elem` ["t", "true", "y", "yes", "1"]
    then return True
    else if normalized `elem` ["f", "false", "n", "no", "0"]
      then return False
      else error $ show var <> " had value " <> show val <> "which could not be parsed as a Bool"

getEnvVarBoolWithDefault :: MonadIO m => String -> Bool -> ExceptT String m Bool
getEnvVarBoolWithDefault var def = do
  exists <- isJust <$> liftIO (lookupEnv var)
  if exists
    then getEnvVarBool var
    else do
      liftIO $ putStrLn $ "Defaulting " <> show var <> " to " <> show def <> " as it was not set in the environment"
      return def

makeConfig :: MonadIO m => ExceptT String m a -> m a
makeConfig a = do
  econf <- runExceptT a
  either error return econf

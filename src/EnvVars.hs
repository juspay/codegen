module EnvVars where

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

gatewayPath :: IO String
gatewayPath = fromMaybe "" <$> lookupEnv "GATEWAY_REPO_PATH"

dbTypesPath :: IO String
dbTypesPath = fromMaybe "" <$> lookupEnv "DB_REPO_PATH"
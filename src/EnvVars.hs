module EnvVars where

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

gatewayPath :: IO String
gatewayPath = fromMaybe "/home/chaitanya/Desktop/work/euler-api-gateway/src" <$> lookupEnv "GATEWAY_REPO_PATH"

dbTypesPath :: IO String
dbTypesPath = fromMaybe "/home/chaitanya/Desktop/work/euler-db/src" <$> lookupEnv "DB_REPO_PATH"
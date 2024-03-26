module EnvVars where

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value(String))


gatewayPath :: IO String
gatewayPath = fromMaybe "/home/chaitanya/Desktop/work/euler-api-gateway/src" <$> lookupEnv "GATEWAY_REPO_PATH"

dbTypesPath :: IO String
dbTypesPath = fromMaybe "/home/chaitanya/Desktop/work/euler-db/src" <$> lookupEnv "DB_REPO_PATH"

oaiReqTimeoutSecs :: IO String 
oaiReqTimeoutSecs = fromMaybe "60" <$> lookupEnv "OAI_REQUEST_TIMEOUT"


--- Azure OAI envs
-- azureEntraId :: IO String 
-- azureEntraId 

azureAccountName :: IO String
azureAccountName = fromMaybe "" <$> lookupEnv "AZURE_ACCOUNT_NAME"

azureResourceGroupName :: IO String
azureResourceGroupName = fromMaybe "" <$> lookupEnv "AZURE_RESOURCE_GROUP_NAME"

azureSubscriptionId :: IO String
azureSubscriptionId = fromMaybe "" <$> lookupEnv "AZURE_SUBSCRIPTION_ID"

azureAPIVersion :: IO String
azureAPIVersion = fromMaybe "" <$> lookupEnv "AZURE_API_VERSION"
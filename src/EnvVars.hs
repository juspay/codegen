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

azureGPTDeployName :: IO String
azureGPTDeployName = fromMaybe "pg-gpt" <$> lookupEnv "AZURE_GPT_DEPLOYMENT_NAME"

azureAccountName :: IO String
azureAccountName = fromMaybe "" <$> lookupEnv "AZURE_ACCOUNT_NAME"

azureResourceGroupName :: IO String
azureResourceGroupName = fromMaybe "" <$> lookupEnv "AZURE_RESOURCE_GROUP_NAME"

azureSubscriptionId :: IO String
azureSubscriptionId = fromMaybe "" <$> lookupEnv "AZURE_SUBSCRIPTION_ID"

azureAPIVersion :: IO String
azureAPIVersion = fromMaybe "" <$> lookupEnv "AZURE_API_VERSION"

--- Azure app registration envs
azureTenantId :: IO String
azureTenantId = fromMaybe "" <$> lookupEnv "AZURE_TENANT_ID"

azureAppRegClientId :: IO String
azureAppRegClientId = fromMaybe "" <$> lookupEnv "AZURE_APP_CLIENT_ID"

azureAuthScope :: IO String
azureAuthScope = fromMaybe "https://management.azure.com/.default" <$> lookupEnv "AZURE_AUTH_SCOPE"

azureClientSecret :: IO String
azureClientSecret = fromMaybe "" <$> lookupEnv "AZURE_APP_CLIENT_SECRET"

azureGrantType :: IO String
azureGrantType = fromMaybe "client_credentials" <$> lookupEnv "AZURE_GRANT_TYPE"

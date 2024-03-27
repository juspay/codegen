{-# LANGUAGE TypeApplications, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Azure where

import Types 
import EnvVars
import Network.HTTP.Client 
import Data.ByteString.Char8 (ByteString, pack)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types ( Status(statusCode, statusMessage), ok200, HeaderName, Method, created201 )
import Data.Aeson
import Types (AzureLoginResponse(access_token), CreateDeploymentRequest (modelName, version), OaiCreateDeploymentRequest (sku))

-- PUT https://management.azure.com/subscriptions/{subscriptionId}/resourceGroups/{resourceGroupName}/providers/Microsoft.CognitiveServices/accounts/{accountName}/deployments/{deploymentName}?api-version=2023-05-01
-- createDeploymentReq :: 

getAzureLoginUrl :: IO String 
getAzureLoginUrl = do 
    tenant_id <- azureTenantId
    pure $ "https://login.microsoftonline.com/" ++ tenant_id ++ "/oauth2/v2.0/token"

getAzureOAIModelDeploymentUrl :: String  -> IO String 
getAzureOAIModelDeploymentUrl deploymentName = do 
    accountName <- azureAccountName 
    resourceGroup <- azureResourceGroupName 
    subsId <- azureSubscriptionId 
    apiVersion <- azureAPIVersion

    pure $ "https://management.azure.com/subscriptions/" ++ subsId ++ "/resourceGroups/"++ resourceGroup ++"/providers/Microsoft.CognitiveServices/accounts/"++ accountName ++"/deployments/"++ deploymentName ++"?api-version="++apiVersion

getUrlEncodedBody :: IO [(ByteString, ByteString)]
getUrlEncodedBody = do 
    client_id <- azureAppRegClientId
    scope <- azureAuthScope 
    client_secret <- azureClientSecret
    grant_type <- azureGrantType
    pure [(pack "client_id", pack client_id), 
            (pack "scope", pack scope),
            (pack "client_secret", pack client_secret),
            (pack "grant_type", pack grant_type)]


getAzureAuthToken :: IO (Either (Int, String) String)
getAzureAuthToken = do
    url <- getAzureLoginUrl
    body <- getUrlEncodedBody
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest url
    
    let request = urlEncodedBody body initialRequest
    response <- httpLbs request manager

    let statusCode' = responseStatus response
    
    if statusCode' == ok200
      then case eitherDecode $ responseBody response of
            Left err -> return $ Left (400, "AZURE_LOGIN_ERROR" ++ err)
            Right res -> do
              return $ Right $ access_token ( res :: AzureLoginResponse)
      else return $ Left (statusCode statusCode',show $ statusMessage statusCode')


createModifyDeploymentHeader :: String -> [(HeaderName , ByteString) ]
createModifyDeploymentHeader token = [("Content-Type", "application/json"), ("Authorization", pack $ "Bearer "++ token)]

transformDeployReq :: CreateDeploymentRequest  -> OaiCreateDeploymentRequest 
transformDeployReq CreateDeploymentRequest{..} = do 
    let sku = OaiDeployModelSKU "Standard" 1
    let props = OaiDeployProperties $ OaiDeployModelProperties "OpenAI" modelName version 
    OaiCreateDeploymentRequest sku props


modifyDeployment :: Method  -> CreateDeploymentRequest -> IO (CreateDeploymentRes)
modifyDeployment reqMethod req = do 
    accessToken <- getAzureAuthToken
    case accessToken of
        Left res -> pure $ CreateDeploymentFailure (snd res) (fst res)
        Right token -> do
            let headers = createModifyDeploymentHeader token
            let reqBody = transformDeployReq req

            dpmtName <- azureGPTDeployName
            url <- getAzureOAIModelDeploymentUrl dpmtName
            manager <- newManager tlsManagerSettings
            initialRequest <- parseRequest url

            let request = initialRequest{
                method = reqMethod,
                requestHeaders = headers,
                requestBody = RequestBodyLBS $ encode reqBody
            }

            response <- httpLbs request manager

            let statusCode' = responseStatus response
            
            if statusCode' == created201
            then return CreateOrDeleteDeploymentSuccess 
            else return $ CreateDeploymentFailure "CREATE_DEPLOYMENT_ERROR" 500



createDeployment :: CreateDeploymentRequest -> IO (CreateDeploymentRes) 
createDeployment = modifyDeployment "PUT"

deleteDeployment :: CreateDeploymentRequest -> IO (CreateDeploymentRes) 
deleteDeployment = modifyDeployment "DELETE"
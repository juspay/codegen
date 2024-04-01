{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, TypeApplications, DataKinds #-}

module OpenAIIntraction where

import Network.HTTP.Client
import Network.HTTP.Client (Manager(..))
import GHC.Records (getField)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.List.Extra
import GHC.Generics (Generic)
import Control.Exception (catch, throwIO)
import Network.HTTP.Types.Status (statusCode,statusMessage , ok200)
import Examples
import Types 
import EnvVars (oaiReqTimeoutSecs)

generatePrompt docData modName inputs outputs =
  "Generate a Haskell code to transform data into the API request body using the provided information. Utilize Haskell types if mentioned below. If no request body is specified, skip generating the transformation function. Do not create Haskell data types for request and response bodies; assume they already exist.\nDESCRIPTION:" ++ docData ++ (maybe "\n" (\x -> "\nMODULE_NAME:" ++ x) modName) ++ "\nINPUT TYPE:" ++ inputs ++ "\nOUTPUT TYPE:" ++ outputs

generatePromptForRoutes docData gatewayRequestType gatewatResponseType =
  "Generate Types for this data\n" ++ docData ++ "\nGateway Request Type : " ++ gatewayRequestType ++ "\nGateway Response Type : " ++ gatewatResponseType

generatePromptForTypes = "Generate open api spec from the data given below. Make sure to add required:True if the field is mandatory. Give examples only for static fields, that is if the value of field never change\n\n"

baseURL = ""
baseGPTUrl = ""
deploymentForTrans = ""
deploymentForTypes = ""
apiVersion = ""
deploymentType = ""
apiKey = ""
gptapikey = ""

typesRequest :: DocData -> IO (Either (Int,String) String)
typesRequest prompt = do
  let url = baseURL ++ "openai/deployments/" ++ deploymentForTrans ++ deploymentType ++ "?api-version=" ++ apiVersion
      headers = [("Content-Type", "application/json"), ("api-key", pack apiKey)]
      promptMsg = typeSample ++ [Message "user" (generatePromptForTypes ++ getField @"document_data" prompt)]
  requestGPT 0.01 promptMsg url headers "open-api.yml"


formatDocument :: DocData -> IO (FormatedDocData)
formatDocument prompt = do
  let url = baseGPTUrl ++ "openai/deployments/" ++ deploymentForTypes ++ deploymentType ++ "?api-version=" ++ apiVersion
      headers = [("Content-Type", "application/json"), ("api-key", pack gptapikey)]
      promptMsg = [Message "user" ("Convert the following document data in csv format considering next line will be new field\n" <> (getField @"document_data" prompt))]
  gptoutput <- requestGPT 0.01 promptMsg url headers "txt.csv"
  case gptoutput of
    Right codeInput -> pure $ FormatedDocData codeInput
    Left (statusCode, statusMessage) -> throwIO $ ErrorResponse statusCode statusMessage

transformsRequest :: Double -> String -> String -> IO (Either (Int,String) String)
transformsRequest temp prompt inputType = do
    let promptMsg = (ragMessages inputType) ++ [Message "user" prompt]
        url = baseURL ++ "openai/deployments/" ++ deploymentForTrans ++ deploymentType ++ "?api-version=" ++ apiVersion
        headers = [("Content-Type", "application/json"), ("api-key", pack apiKey)]
    requestGPT temp promptMsg url headers "Response.hs"

routesRequest :: RoutesInput -> IO RoutesOutput
routesRequest prompt = do
  let url = baseURL ++ "openai/deployments/" ++ deploymentForTrans ++ deploymentType ++ "?api-version=" ++ apiVersion
      headers = [("Content-Type", "application/json"), ("api-key", pack apiKey)]
      promptMsg = routesSample ++ [Message "user" (generatePromptForRoutes (getField @"documentData" prompt) (getField @"gatewayReqType" prompt) (getField @"gatewayRespType" prompt))]
  gptoutput <- requestGPT 0.01 promptMsg url headers "Routes.hs"
  case gptoutput of
    Right codeInput -> pure $ RoutesOutput codeInput
    Left (statusCode, statusMessage) -> throwIO $ ErrorResponse statusCode statusMessage

getTimeoutInMicroSec :: String  -> Int 
getTimeoutInMicroSec timeInSec = read timeInSec  * 1000000

-- requestGPT :: [Message] -> IO (Either (Int,String) String)
requestGPT temp promptMsg url headers fileName = do
    manager <- newManager tlsManagerSettings
    timeoutSecs <- oaiReqTimeoutSecs
    writeFile "promptmsg" (show promptMsg)
    let request_body = LLMRequestBody
          { messages = promptMsg
          , max_tokens = 1000
          , temperature = temp
          , frequency_penalty = 0.2
          , presence_penalty = 0.5
          , top_p = 0.95
          , stop = Nothing
          }

    initialRequest <- parseRequest url
    let request = initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode request_body
          , requestHeaders = headers
          , responseTimeout = responseTimeoutMicro $ getTimeoutInMicroSec timeoutSecs
          }

    response <- httpLbs request manager
    writeFile "Response" (show response)
    let statusCode' = responseStatus response
    if statusCode' == ok200
      then case eitherDecode $ responseBody response of
            Left err -> return $ Left $ (400,"OPEN_API_DECODE_ERROR")
            Right res -> do
              let resContents = (unlines $ map (\x ->
                    if "module " `isPrefixOf` x then "module Response where" else x
                    ) $ lines $ (getField @"content") $ ((message $ head $ choices (res :: ResponseBody)) :: MessageContent))
              writeFile fileName resContents
              return $ Right ((getField @"content") $ ((message $ head $ choices (res :: ResponseBody)) :: MessageContent))
      else return $ Left $ (statusCode $ statusCode',show $ statusMessage $ statusCode')

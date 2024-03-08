{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, TypeApplications, DataKinds #-}

module OpenAPIIntreaction where

import Network.HTTP.Client
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

generatePrompt docData modName inputs outputs =
  "Generate a Haskell code to transform data into the API request body using the provided information. Utilize Haskell types if mentioned below. If no request body is specified, skip generating the transformation function. Do not create Haskell data types for request and response bodies; assume they already exist.\nDESCRIPTION:" ++ docData ++ (maybe "\n" (\x -> "\nMODULE_NAME:" ++ x) modName) ++ "\nINPUT TYPE:" ++ inputs ++ "\nOUTPUT TYPE:" ++ outputs


-- https://genius-gpt-4.openai.azure.com/openai/deployments/genius-gpt-4-turbo/chat/completions?api-version=2024-02-15-preview
-- https://gateway-integration-ai.openai.azure.com/openai/deployments/v6-gateway/chat/completions?api-version=2024-02-15-preview"
baseURL = ""
baseGPTUrl = ""
deploymentForTrans = ""
deploymentForTypes = ""
apiVersion = ""
deploymentType = ""
apiKey = ""
gptapikey = ""

typesRequest :: String -> IO (String)
typesRequest prompt = do
  expromt <- readFile "./testprompt"
  let url = baseURL ++ "openai/deployments/" ++ deploymentForTrans ++ deploymentType ++ "?api-version=" ++ apiVersion
      headers = [("Content-Type", "application/json"), ("api-key", pack apiKey)]
      promptMsg = typeSample ++ [Message "user" expromt]
  gptoutput <- requestGPT promptMsg url headers
  case gptoutput of
    Right codeInput -> pure codeInput
    Left (statusCode, statusMessage) -> throwIO $ ErrorResponse statusCode statusMessage

formatDocument :: DocData -> IO (FormatedDocData)
formatDocument prompt = do
  -- expromt <- readFile "./testprompt"
  let url = baseGPTUrl ++ "openai/deployments/" ++ deploymentForTypes ++ deploymentType ++ "?api-version=" ++ apiVersion
      headers = [("Content-Type", "application/json"), ("api-key", pack gptapikey)]
      promptMsg = [Message "user" ("Convert the following document data in csv format considering next line will be new field\n" <> (getField @"document_data" prompt))]
  gptoutput <- requestGPT promptMsg url headers
  case gptoutput of
    Right codeInput -> pure $ FormatedDocData codeInput
    Left (statusCode, statusMessage) -> throwIO $ ErrorResponse statusCode statusMessage

transformsRequest :: String -> String -> IO (Either (Int,String) String)
transformsRequest prompt inputType = do
    let promptMsg = (ragMessages inputType) ++ [Message "user" prompt]
        url = baseURL ++ "openai/deployments/" ++ deploymentForTrans ++ deploymentType ++ "?api-version=" ++ apiVersion
        headers = [("Content-Type", "application/json"), ("api-key", pack apiKey)]
    requestGPT promptMsg url headers

-- requestGPT :: [Message] -> IO (Either (Int,String) String)
requestGPT promptMsg url headers = do
    manager <- newManager tlsManagerSettings
    writeFile "promptmsg" (show promptMsg)
    let request_body = LLMRequestBody
          { messages = promptMsg
          , max_tokens = 1000
          , temperature = 0.01
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
              writeFile "Response.hs" resContents
              return $ Right ((getField @"content") $ ((message $ head $ choices (res :: ResponseBody)) :: MessageContent))
      else return $ Left $ (statusCode $ statusCode',show $ statusMessage $ statusCode')
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GenerateTypes where

import OpenAPI.Generate
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAPI.Generate.IO as OAI
import qualified OpenAPI.Generate.OptParse as OAO
import qualified OpenAPI.Generate.Types as OAT
import System.Exit
import Data.List (isInfixOf)
import Types
import OpenAIIntraction
import Control.Exception (throwIO)

generateTypesFromSpec :: DocData -> IO FormatedDocData
generateTypesFromSpec docData = do
    gptoutput <- pure $ Right "" -- typesRequest docData
    case gptoutput of
        Right codeInput -> do
            settings <- OAO.getSettings
            spec <- decodeOpenApi "open-api.yml"
            outFiles@OAI.OutputFiles {..} <- OAI.generateFilesToCreate False spec settings
            let allModFiles = OAI.outputFilesModuleFiles outFiles
                typesFiles = filter (\(x,y) -> "Types.hs" `isInfixOf` x ) allModFiles
            pure $ FormatedDocData $ snd $ head typesFiles
        Left (statusCode, statusMessage) -> throwIO $ ErrorResponse statusCode statusMessage

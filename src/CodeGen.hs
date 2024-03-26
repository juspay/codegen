{-# LANGUAGE ScopedTypeVariables, TypeApplications, DataKinds #-}
{-# LANGUAGE BangPatterns #-}


module CodeGen where


import Control.Exception
import GHC.Records (getField)
import qualified Data.HashMap.Strict as HM

import Types
import OpenAPIIntreaction
import CheckCodeAnalysis


generateTransformFuncPrompt docData modName inputs outputs =
  "Generate a Haskell code to transform data into the API request body using the provided information."
  ++ "Utilize Haskell types if mentioned below. If no request body is specified, skip generating the transformation function." 
  ++ "Do not create Haskell data types for request and response bodies; assume they already exist."
  ++ "\nDESCRIPTION:" ++ docData ++ (maybe "\n" (\x -> "\nMODULE_NAME:" ++ x) modName)
  ++ "\nINPUT TYPE:" ++ inputs ++ "\nOUTPUT TYPE:" ++ outputs




generateTransformFunctions :: (HM.HashMap String [String]) -> (HM.HashMap String [String]) -> CodeInput -> IO CodeOutput
generateTransformFunctions allFields dbFields codeInput = do
    let prompt = generateTransformFuncPrompt (getField @"document_data" codeInput) (module_name codeInput) (concat $ inputs codeInput) (output codeInput)
    writeFile "testprompt" prompt
    !genResponse <- transformsRequest prompt (concat $ inputs codeInput)

    case genResponse of 
        Left (statusCode, statusMessage) -> 
            -- TODO: Add retry mechanism for ratelimit
            throwIO $ ErrorResponse statusCode statusMessage
        Right genFunction -> do 
            cf <- compareASTForFuns allFields dbFields codeInput genFunction

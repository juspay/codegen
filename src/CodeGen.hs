{-# LANGUAGE ScopedTypeVariables, TypeApplications, DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}


module CodeGen where


import Control.Exception
import GHC.Records (getField)
import qualified Data.HashMap.Strict as HM

import Types
import OpenAPIIntreaction
import CheckCodeAnalysis
import Examples (ragMessages)
import Data.List (intercalate)


generateTransformFuncPrompt docData modName inputs outputs =
  "Generate a Haskell code to transform data into the API request body using the provided information."
  ++ "Utilize Haskell types if mentioned below. If no request body is specified, skip generating the transformation function." 
  ++ "Do not create Haskell data types for request and response bodies; assume they already exist."
  ++ "\nDESCRIPTION:" ++ docData ++ (maybe "\n" (\x -> "\nMODULE_NAME:" ++ x) modName)
  ++ "\nINPUT TYPE:" ++ inputs ++ "\nOUTPUT TYPE:" ++ outputs

reaskHallucinatedFuncsPrompt halucinatedFuns = 
    "Following imported functions are not available in codebase: " ++ halucinatedFuns
    ++ "\n Can you generate function with it's functionality and add it to previously generated code?"


data FuncReaskInput = FuncReaskInput{
    retryCount :: Int,
    convHistory :: [Message],
    allFields :: (HM.HashMap String (String,[String])), 
    dbFields :: (HM.HashMap String (String,[String])),
    codeInput :: CodeInput
}


funcReaskPipeline :: FuncReaskInput -> IO CodeOutput
funcReaskPipeline fri@FuncReaskInput{..} = do 
    !genResponse <- transformsRequest convHistory 

    case genResponse of 
        Left (statusCode, statusMessage) -> 
            -- TODO: Add retry mechanism for ratelimit
            throwIO $ ErrorResponse statusCode statusMessage
        Right genFunction -> do 
            cf <- compareASTForFuns allFields dbFields codeInput genFunction
            case cf of 
                Left err -> return $ CodeOutput err [] 0.0 0.0 HM.empty
                Right co@CodeOutput{..} -> do
                    if retryCount - 1 == 0 
                        then return co
                        else do
                            let cH = convHistory ++ [Message "assistant" code, Message "user" $ intercalate ", " hallucinated_functions]
                            funcReaskPipeline fri{convHistory=cH, retryCount=retryCount-1} 



generateTransformFunctions :: (HM.HashMap String (String,[String])) -> (HM.HashMap String (String,[String])) -> CodeInput -> IO CodeOutput
generateTransformFunctions allFields dbFields codeInput = do
    let prompt = generateTransformFuncPrompt (getField @"document_data" codeInput) (module_name codeInput) (concat $ inputs codeInput) (output codeInput)
    writeFile "testprompt" prompt
    let promptMsg = ragMessages (concat $ inputs codeInput) ++ [Message "user" prompt]
    
    funcReaskPipeline $ FuncReaskInput 3 promptMsg allFields dbFields codeInput
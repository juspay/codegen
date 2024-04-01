{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Server where

import Servant.Server
import qualified Network.Wai.Handler.Warp
import Servant.API ((:>), JSON, ReqBody, (:<|>), Post, PlainText)
import Servant (Proxy(..),  (:<|>)(..))
import Types
import qualified Data.HashMap.Strict as HM
import EnvVars
import qualified Language.Haskell.Tools.Parser.RemoveUnusedFuns as Unused
import ParseTypes
import Data.List
import Data.List.Extra (replace, stripSuffix, foldl)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import CheckCodeAnalysis
import OpenAIIntraction
import Control.Monad.IO.Class
import GenerateFlow
import Azure (createDeployment, deleteDeployment)

import Config (temperatureRanges)
import GHC.Records (getField)
import GenerateTypes

type MyApi = "uploadDoc" :> ReqBody '[JSON] DocumentData :> Post '[JSON] DocumentSplitData
            :<|> "gateway" :> ("integrate" :> ReqBody '[JSON] CodeInput :> Post '[JSON] CodeOutput
                              :<|> "types" :> ReqBody '[JSON] DocData :> Post '[JSON] FormatedDocData
                              :<|> "routes" :> ReqBody '[JSON] RoutesInput :> Post '[JSON] RoutesOutput
                              :<|> "flows" :> ReqBody '[JSON] FlowInput :> Post '[JSON] FlowOutput)
            :<|> "document" :> ("format" :> ReqBody '[JSON] DocData :> Post '[JSON] FormatedDocData)
            :<|> "deployment" :> ("create" :> ReqBody '[JSON] CreateDeploymentRequest  :> Post '[JSON] CreateDeploymentRes
                                   :<|> "delete" :> ReqBody '[JSON] CreateDeploymentRequest  :> Post '[JSON] CreateDeploymentRes)


server :: ((HM.HashMap String (String,[String])), (HM.HashMap String (String,[String]))) -> Server MyApi
server allTypes = splitDoc :<|> (genTransForms :<|> genTypes :<|> genRoutes :<|> genFlow) :<|> (formatDoc) :<|> (createDep :<|> deleteDep)
  where genTransForms codeInput = liftIO $ generateTransformFuns codeInput allTypes
        formatDoc docData = liftIO $ formatDocument docData
        genTypes docData = liftIO $ generateTypesFromSpec docData
        genRoutes input = liftIO $ routesRequest input
        genFlow docData = do
            liftIO $ print docData
            pure $ generateInstances docData
        splitDoc book = undefined
        createDep req = liftIO $ createDeployment req
        deleteDep req = liftIO $ deleteDeployment req


myApi :: Proxy MyApi
myApi = Proxy

app :: ((HM.HashMap String (String,[String])), (HM.HashMap String (String,[String]))) -> Application
app allTypes val x = do
    serve myApi (server allTypes) val x

main :: IO ()
main = do
    allTypes <- getAllTypesParsed
    print "Starting Server..."
    Network.Wai.Handler.Warp.run 3005 (app allTypes)

filteredMods = ["Euler.DB.Mesh.UtilsTh"]

getAllTypesParsed :: IO ((HM.HashMap String (String,[String])), (HM.HashMap String (String,[String])))
getAllTypesParsed = do
    repoPath <- dbTypesPath
    gateway <- gatewayPath
    let dbCond val = val `elem` filteredMods
    let gatewayCond val = not $ isPrefixOf "Euler.API.Gateway.Domain" val || isPrefixOf "Euler.API.Gateway.Types" val
    gatewayTypes <- getTypes gateway gatewayCond
    dbTypes <- getTypes repoPath dbCond
    pure (gatewayTypes, dbTypes)

getTypes :: String -> ([Char] -> Bool) -> IO (HM.HashMap String (String,[String]))
getTypes repoPath cond = do
    let allSubFiles = Unused.getAllSubFils repoPath []
    y <- foldM (\acc t -> do
           let modName = (fromMaybe "" $ stripSuffix ".hs" $ replace "/" "." $ fromMaybe "" $ stripPrefix (repoPath ++ "/") t)
           if not $ cond modName
           then do
            hm <- parseAndGetTypes repoPath modName
            pure $ acc <> hm
           else pure acc) HM.empty allSubFiles
    pure y

generateTransformFuns codeInput (gatewayTypes,dbTypes) = do
    print codeInput
    let prompt = generatePrompt (getField @"document_data" codeInput) (module_name codeInput) (concat $ inputs codeInput) (output codeInput)
    changedInput <- mapM (\temp -> transformsRequest temp prompt (concat $ inputs codeInput)) (temperatureRanges)
    allOutputs <- mapM (\input -> compareASTForFuns input gatewayTypes dbTypes codeInput) changedInput
    pure $ head $ sortOn (\codeOutput -> hallucinations_functions_score codeOutput + hallucinations_types_score codeOutput) allOutputs

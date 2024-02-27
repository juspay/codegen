{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Server where

import Servant.Server
import qualified Network.Wai.Handler.Warp
import Servant.API ((:>), JSON, ReqBody, (:<|>), Post)
import Servant (Proxy(..))
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

type MyApi = "uploadDoc" :> ReqBody '[JSON] DocumentData :> Post '[JSON] DocumentSplitData

server :: Server MyApi
server = postBook
  where listAllBooks = undefined
        postBook book = undefined

myApi :: Proxy MyApi
myApi = Proxy

app :: Application
app val x = do
    allTypes <- getAllTypesParsed

    serve myApi server val x

main :: IO ()
main = Network.Wai.Handler.Warp.run 8080 app

filteredMods = ["Euler.DB.Mesh.UtilsTh"]

getAllTypesParsed :: IO (HM.HashMap String [String])
getAllTypesParsed = do
    repoPath <- dbTypesPath
    gateway <- gatewayPath
    let dbCond val = val `elem` filteredMods
    let gatewayCond val = not $ isPrefixOf "Euler.API.Gateway.Domain" val || isPrefixOf "Euler.API.Gateway.Types" val
    gatewayTypes <- getTypes gateway gatewayCond
    dbTypes <- getTypes repoPath dbCond
    pure $ gatewayTypes <> dbTypes

getTypes :: String -> ([Char] -> Bool) -> IO (HM.HashMap String [String])
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


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, DataKinds #-}

module CheckCodeAnalysis where

import qualified Language.Haskell.Tools.Parser.FlowChange as Fl
import qualified Data.HashMap.Strict as HM
import Language.Haskell.Tools.Parser.ParseModule
import Data.Maybe (mapMaybe)
import Language.Haskell.Tools.AST
import GHC hiding (Name, mkModuleName)
import Data.List (nub, isInfixOf)
import GHC.Float (int2Float)
import Data.Char (isAlphaNum, toLower)
import Control.Reference
import qualified Language.Haskell.Tools.Parser.RemoveUnusedFuns as Unused
import Types
import OpenAPIIntreaction
import Control.Exception
import GHC.Records (getField)
import Language.Haskell.Tools.Refactor as HT
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.PrettyPrint
import System.Directory
import System.IO.Unsafe
import System.Environment
import Data.List.Extra (replace)

accDetails = HM.fromList [] -- [("accountdetails",["onepayMerchantId","onepayMerchantName","onepayApiKey"])]

fewMappings =
    [""]

getModuleName :: Ann UModuleName (Dom GhcPs) SrcTemplateStage -> Maybe String
getModuleName (Ann _ (UModuleName ex)) = Just ex

checkIfModPresent :: String  -> Ann UImportDecl (Dom GhcPs) SrcTemplateStage -> Bool
checkIfModPresent srcDir expr@(Ann _ (UImportDecl _ _ _ _ modName qualifiedName specs)) =
    not ("Euler.API.Gateway" `isInfixOf` (fromMaybe "" $ getModuleName modName)) || (unsafePerformIO $ doesFileExist (srcDir <> (replace "." "/" $ fromMaybe "" $ getModuleName modName) <> ".hs"))

compareASTForFuns :: (HM.HashMap String (String,[String])) -> (HM.HashMap String (String,[String])) -> CodeInput -> IO CodeOutput
compareASTForFuns allFields dbFields codeInput = do
    let prompt = generatePrompt (getField @"document_data" codeInput) (module_name codeInput) (concat $ inputs codeInput) (output codeInput)
    writeFile "testprompt" prompt
    !changedInput <- transformsRequest prompt (concat $ inputs codeInput)
    -- let allSubFiles = Unused.getAllSubFils ""  []
    case changedInput of
        Right genCode -> do
            codegenDir <- getEnv "CODEGEN_DIR"
            emoduleAST <- try $ moduleParser codegenDir "Response"
            case emoduleAST of
              Right moduleAST -> do
                allFuns <- Fl.getModFunctionList moduleAST "Response"
                let requestType = (inputs codeInput) !! 0
                let pats = mapMaybe getAllDot (moduleAST ^? biplateRef)
                    allDotOps = nub $ (snd <$> pats) ++ (fst <$> pats)
                print allDotOps
                let filteredFuns = nub $ map (\(x,list) -> (x, filter (\(_,name) -> not $ name `elem` allDotOps ) list)) (allFuns :: [(String,[(String,String)])])
                    allFunsInvolved = nub $ filter (\(x,y) -> not $ x == "") (concat $ snd <$> allFuns)
                    getAllLocalFuns = nub $ filter (\(x,y) -> "Euler.API.Gateway" `isInfixOf` x) (concat $ snd <$> allFuns)
                srcDir <- getEnv "SRC_DIR"
                filesToParseAST <- mapM (\x -> do
                                    emod <- try $ moduleParser srcDir x
                                    case emod of
                                        Right mod -> pure (Just mod,x)
                                        Left (e :: SomeException) -> pure (Nothing,x)) $ nub $ fst <$> getAllLocalFuns
                allLocalFunList <- mapM (\(maybeMod,x) -> maybe (pure []) (\mod -> Fl.getModFunctionList mod x) maybeMod) filesToParseAST
                let impsFil = filter (checkIfModPresent srcDir) (moduleAST ^? modImports & annList :: [HT.ImportDecl'])
                (AST.AnnListG annot currentDecl) <- moduleAST ^? (modDecl)
                let modifiedAST = (.=) modImports (AST.AnnListG annot impsFil) moduleAST
                let onlyFuns = (((fst <$> (concat allLocalFunList))))
                    totalFuns = length allFunsInvolved
                    halucinatedFuns = filter (\x -> not $ x `elem` onlyFuns) (nub $ snd <$> getAllLocalFuns)
                    halucinatedFunScore = ((int2Float $ length halucinatedFuns) / int2Float totalFuns ) * 100
                    allDataTypes = (dbFields <> allFields <> accDetails)
                    allNotRelatedFields = filter (\(x,y) -> not $ case HM.lookup ( if x == "request" then (toLower <$> requestType) else toLower <$> x) (allDataTypes) of
                                                            Just val -> y `elem` (snd val)
                                                            Nothing -> False) pats
                    halucinatedTypeScore = ((int2Float $ length allNotRelatedFields) / int2Float (length pats) ) * 100
                    toBeFound = snd <$> allNotRelatedFields
                    getFilteredFromAllDBFields = filter (\(x,(_,y)) -> any (\val -> val `elem` y) toBeFound ) $ HM.toList dbFields
                    requestTypeFields = snd $ fromMaybe ("",[]) $ HM.lookup (toLower <$> requestType) allFields
                    mapAllRelatedFields = HM.fromList $ map (\(x,y) -> let expectedFields = if x == "request" then []
                                                                                else foldl (\acc (typeName,(_,fields)) -> if y `elem` fields && typeName `elem` ( map (\x -> toLower <$> x) $ requestTypeFields) then
                                                                                        let orgTypeName = filter (\x -> (toLower <$> x) == typeName) requestTypeFields
                                                                                        in acc ++ orgTypeName else acc) [] getFilteredFromAllDBFields
                                                        in ((x ++ "." ++ y),expectedFields)) allNotRelatedFields
                pure $ CodeOutput (prettyPrint modifiedAST) halucinatedFuns halucinatedFunScore halucinatedTypeScore mapAllRelatedFields
              Left (e :: SomeException) ->
                pure $ CodeOutput ("Couldnt parse the below code, so metrics are not generated\n" <> genCode) [] 0.0 0.0 HM.empty
        Left (statusCode, statusMessage) -> throwIO $ ErrorResponse statusCode statusMessage

getAllDot :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> Maybe (String,String)
getAllDot expr@(Ann _ (UInfixApp (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName (AnnListG _ names) (Ann _ (UNamePart left)))))))) (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ".")))))) (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ name) (Ann _ (UNamePart right)))))))))) = if null names && null name then Just $ (left, right) else Nothing
getAllDot expr@(Ann _ (UInfixApp (Ann _ (UApp _ (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ names) (Ann _ (UNamePart left)))))))))) (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ".")))))) (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ name) (Ann _ (UNamePart right))))))))  )) = if null names && null name then Just $ (left, right) else Nothing
getAllDot expr@(Ann _ (UInfixApp (Ann _ (UInfixApp _ _ (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ names) (Ann _ (UNamePart left)))))))))) (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ".")))))) (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ name) (Ann _ (UNamePart right)))))))))) = if null names && null name then Just $ (left, right) else Nothing
getAllDot expr = Nothing

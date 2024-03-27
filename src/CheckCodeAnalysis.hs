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

import System.Environment

accDetails = HM.fromList [] -- [("accountdetails",["onepayMerchantId","onepayMerchantName","onepayApiKey"])]

fewMappings =
    [""]

compareASTForFuns :: (HM.HashMap String [String]) -> (HM.HashMap String [String]) -> CodeInput -> String -> IO (Either String CodeOutput)
compareASTForFuns allFields dbFields codeInput genCode = do
    codegenDir <- getEnv "CODEGEN_DIR"
    emoduleAST <- try $ moduleParser codegenDir "Response"
    case emoduleAST of
        Right moduleAST -> do
            allFuns <- Fl.getModFunctionList moduleAST "Sample"
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
            let onlyFuns = (((fst <$> (concat allLocalFunList))))
                totalFuns = length allFunsInvolved
                halucinatedFuns = filter (\x -> not $ x `elem` onlyFuns) (nub $ snd <$> getAllLocalFuns)
                halucinatedFunScore = ((int2Float $ length halucinatedFuns) / int2Float totalFuns ) * 100
                allNotRelatedFields = filter (\(x,y) -> not $ case HM.lookup ( if x == "request" then requestType else toLower <$> x) (allFields <> accDetails) of
                                                        Just val -> y `elem` val
                                                        Nothing -> False) pats
            let halucinatedTypeScore = ((int2Float $ length allNotRelatedFields) / int2Float (length pats) ) * 100
            let toBeFound = snd <$> allNotRelatedFields
                getFilteredFromAllFields = filter (\(x,y) -> any (\val -> val `elem` y) toBeFound ) $ HM.toList allFields
                getFilteredFromAllDBFields = filter (\(x,y) -> any (\val -> val `elem` y) toBeFound ) $ HM.toList dbFields
                mapAllRelatedFields = HM.fromList $ map (\(x,y) -> let expectedFields = if x == "request" then foldl (\acc (typeName,fields) -> if y `elem` fields then acc ++ [typeName] else acc) [] getFilteredFromAllFields
                                                                            else foldl (\acc (typeName,fields) -> if y `elem` fields then acc ++ [typeName] else acc) [] getFilteredFromAllDBFields
                                                    in ((x ++ "." ++ y),expectedFields)) allNotRelatedFields
            writeFile "AllFuns" (show pats)
            return $ Right $ CodeOutput genCode halucinatedFuns halucinatedFunScore halucinatedTypeScore mapAllRelatedFields
        Left (e :: SomeException) ->
            return $ Left $ "Couldnt parse the below code, so metrics are not generated\n" <> genCode



getAllDot :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> Maybe (String,String)
getAllDot expr@(Ann _ (UInfixApp (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName (AnnListG _ names) (Ann _ (UNamePart left)))))))) (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ".")))))) (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ name) (Ann _ (UNamePart right)))))))))) = if null names && null name then Just $ (left, right) else Nothing
getAllDot expr@(Ann _ (UInfixApp (Ann _ (UApp _ (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ names) (Ann _ (UNamePart left)))))))))) (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ".")))))) (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ name) (Ann _ (UNamePart right))))))))  )) = if null names && null name then Just $ (left, right) else Nothing
getAllDot expr@(Ann _ (UInfixApp (Ann _ (UInfixApp _ _ (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ names) (Ann _ (UNamePart left)))))))))) (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ".")))))) (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ name) (Ann _ (UNamePart right)))))))))) = if null names && null name then Just $ (left, right) else Nothing
getAllDot expr = Nothing

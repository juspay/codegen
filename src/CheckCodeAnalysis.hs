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

requestTypeEnv = pure ""
accDetails = HM.fromList []

fewMappings =
    [""]

compareASTForFuns :: (HM.HashMap String [String]) -> IO ()
compareASTForFuns allFields = do
    moduleAST <- moduleParser "" ""
    allFuns <- Fl.getModFunctionList moduleAST "Sample"
    requestType <- requestTypeEnv
    let pats = mapMaybe getAllDot (moduleAST ^? biplateRef)
        allDotOps = nub $ (snd <$> pats) ++ (fst <$> pats)
    print allDotOps
    let filteredFuns = nub $ map (\(x,list) -> (x, filter (\(_,name) -> not $ name `elem` allDotOps ) list)) (allFuns :: [(String,[(String,String)])])
        allFunsInvolved = nub $ filter (\(x,y) -> not $ x == "") (concat $ snd <$> allFuns)
        getAllLocalFuns = nub $ filter (\(x,y) -> "Euler.API.Gateway" `isInfixOf` x) (concat $ snd <$> allFuns)
    filesToParseAST <- mapM (\x -> do
                          mod <- moduleParser "" x
                          pure (mod,x)) $ nub $ fst <$> getAllLocalFuns
    allLocalFunList <- mapM (\(mod,x) -> Fl.getModFunctionList mod x) filesToParseAST
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
        mapAllRelatedFields = map (\(x,y) -> let expectedFields = foldl (\acc (typeName,fields) -> if y `elem` fields then acc ++ typeName else acc) [] getFilteredFromAllFields
                                              in ((x,y),expectedFields)) allNotRelatedFields
    writeFile "AllFuns" (show pats)
    print totalFuns
    print (nub $ snd <$> allFunsInvolved)
    print (getAllLocalFuns)
    print $ halucinatedFuns
    print allNotRelatedFields
    print getFilteredFromAllFields
    print mapAllRelatedFields
    print halucinatedTypeScore
    print $ halucinatedFunScore

getAllDot :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> Maybe (String,String)
getAllDot expr@(Ann _ (UInfixApp (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName (AnnListG _ names) (Ann _ (UNamePart left)))))))) (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ".")))))) (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ name) (Ann _ (UNamePart right)))))))))) = if null names && null name then Just $ (left, right) else Nothing
getAllDot expr@(Ann _ (UInfixApp (Ann _ (UApp _ (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ names) (Ann _ (UNamePart left)))))))))) (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ".")))))) (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ name) (Ann _ (UNamePart right))))))))  )) = if null names && null name then Just $ (left, right) else Nothing
getAllDot expr@(Ann _ (UInfixApp (Ann _ (UInfixApp _ _ (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ names) (Ann _ (UNamePart left)))))))))) (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart ".")))))) (Ann _ (UVar (Ann _ (UNormalName (Ann _ (UQualifiedName  (AnnListG _ name) (Ann _ (UNamePart right)))))))))) = if null names && null name then Just $ (left, right) else Nothing
getAllDot expr = Nothing
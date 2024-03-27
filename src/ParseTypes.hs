module ParseTypes where

import qualified Language.Haskell.Tools.Parser.SplitTypesInModule as SM
import Language.Haskell.Tools.AST
import GHC hiding (Name, mkModuleName)
import qualified Data.HashMap.Strict as HM
import Data.Char (isAlphaNum, toLower)
import Data.Maybe
import Control.Reference
import Language.Haskell.Tools.Parser.ParseModule (moduleParser)

parseAndGetTypes :: String -> String -> IO (HM.HashMap String (String,[String]))
parseAndGetTypes modPath modName = do
    moduleAST <- moduleParser modPath modName
    pure $ HM.fromList $ mapMaybe getTypeData (moduleAST ^? biplateRef)

getTypeData :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe (String,(String,[String]))
getTypeData expr@(Ann _ (UDataDecl _ _ declHead (AnnListG _ records) _)) = do
    let allFieldNames = concat $ mapMaybe getFieldName records
    (\x -> Just (toLower <$> x,(x,allFieldNames))) =<< (SM.getNameFromDeclHead declHead)
getTypeData _ = Nothing

getFieldName :: Ann UConDecl (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getFieldName expr@(Ann _ (URecordDecl _ _ _ (AnnListG _ fields))) = Just $ concat $ mapMaybe getAllFieldNames fields
getFieldName _ = Nothing

getAllFieldNames :: Ann UFieldDecl (Dom GhcPs) SrcTemplateStage -> Maybe [String]
getAllFieldNames expr@(Ann _ (UFieldDecl (AnnListG _ names) _)) = Just $ mapMaybe getNamePart names

getNamePart :: Ann UName (Dom GhcPs) SrcTemplateStage -> Maybe String
getNamePart expr@(Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart ex)))))) = Just ex
getNamePart expr = Nothing
{-# LANGUAGE QuasiQuotes #-}

module NammaDSL.Lib.Extractor where

import Control.Monad.Extra (fromMaybeM)
import Control.Monad.State
import Data.Aeson
import Data.Bool (bool)
import Data.List (find, intercalate)
import Data.List.Split (wordsBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Interpolate (i)
import Language.Haskell.Exts
import Safe (headMay)
import System.Directory
import Prelude

data EXT_TO = EXT_TO EXT_RT DataName [(FieldName, FieldType)] deriving (Show, Eq, Ord)

data EXT_RT = EXT_NT | EXT_D | EXT_T deriving (Show, Eq, Ord)

type ModName = String

type DataName = String

type FieldName = String

type FieldType = String

data CondLStyle = ENUM | RECORD deriving (Show, Eq, Ord)

-- lets not worry about InfixConDecl for now

data AnalysisState = AnalysisState
  { rootPathPrefix :: [FilePath],
    extImports :: Object,
    haskellImports :: Object,
    remaining :: [FieldType],
    result :: [EXT_TO]
  }
  deriving (Show, Eq, Ord)

type AnalysisM a = StateT AnalysisState IO a

getModuleFilePath :: FilePath -> ModName -> AnalysisM (Maybe FilePath)
getModuleFilePath rootPath moduleName = do
  let partialModulePath = intercalate "/" (wordsBy (== '.') moduleName)
      expectedAbsFilePath = rootPath <> "/" <> partialModulePath <> ".hs"
  fileExists <- liftIO $ doesFileExist expectedAbsFilePath
  pure $ bool Nothing (Just expectedAbsFilePath) fileExists

deepAnalysis :: ModName -> DataName -> AnalysisM [EXT_TO]
deepAnalysis moduleName dName = do
  rootPaths <- gets rootPathPrefix
  correctFilePath <- fromMaybeM (error [i|No Filepath found for module: #{moduleName}|]) $ (headMay . catMaybes) <$> mapM (flip getModuleFilePath moduleName) rootPaths
  parsedHaskellFile <- liftIO $ parseFile correctFilePath
  let decs = case parsedHaskellFile of
        ParseOk (Module _ _ _ _ decl_) -> decl_
        _ -> error [i|Error parsing hs file of module: #{moduleName}|]
      rawEXT_TO = fromMaybe (error [i|Unable to find data type: #{dName} in module #{moduleName}|]) $ findEXT_TO dName decs
  liftIO $ print rawEXT_TO
  -- TODO --
  pure []

findEXT_TO :: DataName -> [Decl SrcSpanInfo] -> Maybe EXT_TO
findEXT_TO dName decls =
  find isTargetDataDecl decls >>= \case
    -- TODO: Check the other possible types. I dont think we need Gadts for now .. might required later.
    TypeDecl _ declHead tp -> do
      let pTp = prettyPrint tp
      pure $ EXT_TO EXT_T (declHeadToString declHead) [("enum", pTp)]
    DataDecl _ dataOrNew _ declHead constructors _ ->
      pure $ EXT_TO (dataOrNewToRecordType dataOrNew) (declHeadToString declHead) (extractCondlInfos constructors)
    _ -> Nothing
  where
    isTargetDataDecl :: Decl SrcSpanInfo -> Bool
    isTargetDataDecl = \case
      (DataDecl _ _ _ (DHead _ dclName) _ _) -> nameToString dclName == dName
      (TypeDecl _ (DHead _ tclName) _) -> nameToString tclName == dName
      _ -> False

    nameToString :: Name l -> String
    nameToString = \case
      Ident _ s -> s
      Symbol _ s -> s

    dataOrNewToRecordType :: DataOrNew l -> EXT_RT
    dataOrNewToRecordType = \case
      DataType _ -> EXT_D
      NewType _ -> EXT_NT

    declHeadToString :: DeclHead SrcSpanInfo -> String
    declHeadToString = \case
      DHead _ dhName -> nameToString dhName
      DHInfix _ _ dhiName -> nameToString dhiName
      DHParen _ declHead -> declHeadToString declHead
      DHApp _ declHead _ -> declHeadToString declHead

    extractCondlInfos :: [QualConDecl l] -> [(FieldName, FieldType)]
    extractCondlInfos qCondDecs
      | all isEnumStyleConDecl qCondDecs = [("enum", (intercalate ",") $ map prettyPrint qCondDecs)]
      | [(QualConDecl _ _ _ (RecDecl _ _ fields))] <- qCondDecs = map extractField fields
      | otherwise = [] -- TODO: Its not the right way to handle this case.
    extractField :: FieldDecl l -> (FieldName, FieldType) -- TODO: Later check for _, might not be required but some corner cases might break
    extractField (FieldDecl _ names tp) = (intercalate "_" (map nameToString names), prettyPrint tp)

    isEnumStyleConDecl :: QualConDecl l -> Bool
    isEnumStyleConDecl (QualConDecl _ _ _ conDecl) = case conDecl of
      ConDecl _ _ _ -> True
      RecDecl _ _ _ -> False
      InfixConDecl _ _ _ _ -> False

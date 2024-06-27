{-# LANGUAGE QuasiQuotes #-}

module NammaDSL.Lib.Extractor where

import Control.Applicative ((<|>))
import Control.Lens hiding (noneOf)
import Control.Monad.Extra (findM, fromMaybeM)
import Control.Monad.State
import Data.Aeson
import Data.Aeson ()
import Data.Aeson.Key (fromString)
import Data.Aeson.Lens (_String)
import Data.Bool (bool)
import Data.List (find, intercalate)
import qualified Data.List.Extra as L
import Data.List.Split (split, splitOn, whenElt, wordsBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set, insert, member)
import Data.String.Interpolate (i)
import Data.Text (unpack)
import qualified Debug.Trace as DT
import Language.Haskell.Exts
import NammaDSL.Utils ()
import Safe (headMay)
import System.Directory
import Text.Regex.PCRE.Heavy (Regex)
import qualified Text.Regex.PCRE.Heavy as RE
import Prelude

data EXT_TO = EXT_TO EXT_RT DataName [(FieldName, FieldType)] deriving (Show, Eq, Ord)

data EXT_RT = EXT_NT | EXT_D | EXT_T deriving (Show, Eq, Ord)

type ModName = String

type DataName = String

type FieldName = String

type FieldType = String

type PotentialModules = [ModName]

-- lets not worry about InfixConDecl for now

data AnalysisState = AnalysisState
  { rootPathPrefix :: [FilePath],
    extImports :: Object,
    haskellImports :: Object,
    dTypes :: [DataName],
    primitives :: [(Regex, String)],
    alreadyNoticedDeepA :: Set (ModName, DataName),
    currentQualifiedImports :: [(String, String)], -- (qualifiedName, mainName)
    remainingEXT_TO :: [EXT_TO],
    tpTinkerer :: (Type SrcSpanInfo -> Type SrcSpanInfo),
    remaining :: [(ModName, DataName)],
    result :: [EXT_TO]
  }

--deriving (Show, Eq, Ord)

type AnalysisM a = StateT AnalysisState IO a

getModuleFilePath :: FilePath -> ModName -> AnalysisM (Maybe FilePath)
getModuleFilePath rootPath moduleName = do
  let partialModulePath = intercalate "/" (wordsBy (== '.') moduleName)
      expectedAbsFilePath = rootPath <> "/" <> partialModulePath <> ".hs"
  fileExists <- liftIO $ doesFileExist expectedAbsFilePath
  pure $ bool Nothing (Just expectedAbsFilePath) fileExists

deepAnalysis :: AnalysisM ()
deepAnalysis = do
  rootPaths <- gets rootPathPrefix
  remaining_ <- gets remaining
  remainingEXT_TO_ <- gets remainingEXT_TO
  tinkerer <- gets tpTinkerer
  unless (null remainingEXT_TO_) $ do
    let nxtEXT_TO = head remainingEXT_TO_
    modify $ \s -> s {remainingEXT_TO = tail remainingEXT_TO_}
    transformedEXT_TO <- mapToExt nxtEXT_TO
    modify $ \s -> s {result = transformedEXT_TO : result s}
    deepAnalysis
  unless (null remaining_) $ do
    let (moduleName, dName) = head remaining_
    modify $ \s -> s {remaining = tail remaining_}
    correctFilePath <- fromMaybeM (error [i|No Filepath found for module: #{moduleName}|]) $ (headMay . catMaybes) <$> mapM (flip getModuleFilePath moduleName) rootPaths
    parsedHaskellFile <- liftIO $ parseFile correctFilePath
    let (imps, decs) = case parsedHaskellFile of
          ParseOk (Module _ _ _ imports_ decl_) -> (imports_, decl_)
          _ -> error [i|Error parsing hs file of module: #{moduleName}|]
        rawEXT_TO = fromMaybe (error [i|Unable to find data type: #{dName} in module #{moduleName}|]) $ findEXT_TO tinkerer dName decs
    parseImportDecls imps
    transformedEXT_TO <- mapToExt rawEXT_TO
    modify $ \s -> s {result = transformedEXT_TO : result s}
    deepAnalysis

parseImportDecls :: [ImportDecl SrcSpanInfo] -> AnalysisM ()
parseImportDecls imps = do
  let qImports = filter importQualified imps
      currentQImps =
        map
          ( \imp ->
              let mainModuleName = L.trim $ prettyPrint (importModule imp)
                  userGivenName = L.trim $ maybe mainModuleName prettyPrint (importAs imp)
               in (userGivenName, mainModuleName)
          )
          qImports
  modify $ \s -> s {currentQualifiedImports = currentQImps}

mapToExt :: EXT_TO -> AnalysisM EXT_TO
mapToExt (EXT_TO rt dn fdls) = do
  mappedFlds <-
    mapM
      ( \(fname, ftype) ->
          if fname == "enum" && rt /= EXT_T
            then do
              let enumTypes = L.trim <$> (splitOn "," ftype)
              mappedEnumTypes <-
                mapM
                  ( \unit -> do
                      let (constructor, remTp) = L.breakOn " " unit
                      mappedRemTp <- mapFieldType remTp
                      pure $ constructor <> " " <> mappedRemTp
                  )
                  enumTypes
              pure (fname, intercalate "," mappedEnumTypes)
            else
              if not (fname `elem` ["derive", "derive'"])
                then do
                  mappedFieldType <- mapFieldType ftype
                  pure (fname, mappedFieldType)
                else pure (fname, ftype)
      )
      fdls
  pure $ EXT_TO rt dn mappedFlds

mapFieldType :: FieldType -> AnalysisM FieldType
mapFieldType tp = do
  let splitedTp = split (whenElt (`elem` typeDelimiter)) tp
  concat <$> (sequence $ map replacer splitedTp)
  where
    replacer :: String -> AnalysisM FieldType
    replacer word
      | word `elem` ["", ")", "(", " ", "[", "]", ","] = pure word
      | otherwise = mapUnitFieldType word

mapUnitFieldType :: FieldType -> AnalysisM FieldType
mapUnitFieldType tp = do
  primitives_ <- gets primitives
  dTypes_ <- gets dTypes
  extImports_ <- gets extImports
  haskellImports_ <- gets haskellImports
  case findInPrimitives primitives_ <|> findInDTypes dTypes_ <|> findInExtImports extImports_ of
    Just x -> pure x
    Nothing -> findInHaskellImports haskellImports_
  where
    findInPrimitives :: [(Regex, FieldType)] -> Maybe FieldType
    findInPrimitives rgs = snd <$> find ((\rg -> tp RE.=~ rg) . fst) rgs

    findInDTypes :: [DataName] -> Maybe FieldType
    findInDTypes dnms = if tp `elem` dnms then Just tp else Nothing

    findInExtImports :: Object -> Maybe FieldType
    findInExtImports extObj = extObj ^? ix (fromString tp) . _String . to unpack

    isAlreadyNoticedDeepA :: (ModName, DataName) -> AnalysisM Bool
    isAlreadyNoticedDeepA (modName, dName) = member (modName, dName) <$> gets alreadyNoticedDeepA

    findInHaskellImports :: Object -> AnalysisM FieldType
    findInHaskellImports obj = do
      guessedModAndDName <- tryToGuessCorrectMod
      let info = (if isQualified tp then guessedModAndDName <|> Just (extractModuleAndTypeName tp) else Nothing) <|> (obj ^? ix (fromString tp) . _String . to unpack . to (\x -> (x, tp)))
      case info of
        Just nxt@(_, ctp) -> do
          isAlreadyNoticed <- isAlreadyNoticedDeepA nxt
          unless isAlreadyNoticed $ modify $ \s -> s {alreadyNoticedDeepA = insert nxt (alreadyNoticedDeepA s), remaining = remaining s ++ [nxt]}
          pure ctp
        Nothing -> error [i|Field type #{tp} not found in Haskell Imports|]

    tryToGuessCorrectMod :: AnalysisM (Maybe (ModName, DataName))
    tryToGuessCorrectMod = do
      currentQImps <- gets currentQualifiedImports
      let (potentialUserModName, dName) = extractModuleAndTypeName tp
          guessedMods = L.nub $ map snd $ filter ((== potentialUserModName) . fst) currentQImps
      correctModule <- findM (isItCorrectMod dName) guessedMods
      pure $ (,) <$> correctModule <*> pure dName

isItCorrectMod :: DataName -> ModName -> AnalysisM Bool
isItCorrectMod dName potentialMod = do
  rootPaths <- gets rootPathPrefix
  correctFilePaths <- catMaybes <$> mapM (flip getModuleFilePath potentialMod) rootPaths
  correctModP <-
    forM
      correctFilePaths
      ( \correctFilePath -> do
          parsedHaskellFile <- liftIO $ parseFile correctFilePath
          case parsedHaskellFile of
            ParseOk (Module _ _ _ _ decl_) -> pure $ any (isTargetDataDecl dName) decl_
            _ -> pure False
      )
  pure $ or correctModP

isQualified :: FieldType -> Bool
isQualified = elem '.'

extractModuleAndTypeName :: String -> (String, String)
extractModuleAndTypeName qualifiedType =
  let parts' = L.splitOn "." qualifiedType
      moduleName = L.intercalate "." (L.init parts')
      typeName = last parts'
   in (moduleName, typeName)

findEXT_TO :: (Type SrcSpanInfo -> Type SrcSpanInfo) -> DataName -> [Decl SrcSpanInfo] -> Maybe EXT_TO
findEXT_TO tinkerer dName decls =
  find (isTargetDataDecl dName) decls >>= \case
    -- TODO: Check the other possible types. I dont think we need Gadts for now .. might required later.
    TypeDecl _ declHead tp -> do
      let pTp = removeExtraSpace (prettyPrint $ tinkerer tp)
      pure $ EXT_TO EXT_T (declHeadToString declHead) [("enum", pTp)]
    DataDecl _ dataOrNew _ declHead constructors _ ->
      pure $ EXT_TO (dataOrNewToRecordType dataOrNew) (declHeadToString declHead) (extractCondlInfos constructors)
    _ -> Nothing
  where
    removeExtraSpace :: String -> String
    removeExtraSpace = unwords . words

    dataOrNewToRecordType :: DataOrNew SrcSpanInfo -> EXT_RT
    dataOrNewToRecordType = \case
      DataType _ -> EXT_D
      NewType _ -> EXT_NT

    declHeadToString :: DeclHead SrcSpanInfo -> String
    declHeadToString = \case
      DHead _ dhName -> nameToString dhName
      DHInfix _ _ dhiName -> nameToString dhiName
      DHParen _ declHead -> declHeadToString declHead
      DHApp _ declHead _ -> declHeadToString declHead

    extractCondlInfos :: [QualConDecl SrcSpanInfo] -> [(FieldName, FieldType)]
    extractCondlInfos qCondDecs
      | all isEnumStyleConDecl qCondDecs = [("enum", (intercalate ",") $ map (removeExtraSpace . L.trim . prettyPrint . tinkerQualConDecl) qCondDecs)]
      | [(QualConDecl _ _ _ (RecDecl _ _ fields))] <- qCondDecs = map extractField $ fields
      | otherwise = [] -- TODO: Its not the right way to handle this case.
    extractField :: FieldDecl SrcSpanInfo -> (FieldName, FieldType) -- TODO: Later check for _, might not be required but some corner cases might break
    extractField (FieldDecl _ names tp) = (intercalate "_" (map nameToString names), removeExtraSpace $ prettyPrint $ tinkerer $ DT.traceShowId tp)

    tinkerQualConDecl :: QualConDecl SrcSpanInfo -> QualConDecl SrcSpanInfo
    tinkerQualConDecl (QualConDecl l m ctx conDecl) = QualConDecl l m ctx (tinkerConDecl conDecl)

    tinkerConDecl :: ConDecl SrcSpanInfo -> ConDecl SrcSpanInfo
    tinkerConDecl = \case
      ConDecl l nm tps -> ConDecl l nm (map tinkerer tps)
      a@(RecDecl _ _ _) -> a
      InfixConDecl l t1 nm t2 -> InfixConDecl l (tinkerer t1) nm (tinkerer t2)

    isEnumStyleConDecl :: QualConDecl SrcSpanInfo -> Bool
    isEnumStyleConDecl (QualConDecl _ _ _ conDecl) = case conDecl of
      ConDecl _ _ _ -> True
      RecDecl _ _ _ -> False
      InfixConDecl _ _ _ _ -> False

isTargetDataDecl :: DataName -> Decl SrcSpanInfo -> Bool
isTargetDataDecl dName = \case
  (DataDecl _ _ _ (DHead _ dclName) _ _) -> nameToString dclName == dName
  (TypeDecl _ (DHead _ tclName) _) -> nameToString tclName == dName
  _ -> False

nameToString :: Name SrcSpanInfo -> String
nameToString = \case
  Ident _ s -> s
  Symbol _ s -> s

typeDelimiter :: String
typeDelimiter = "() [],"

-- TODOs:
-- Done 1. If while deep analysis we encounter a used defned qualified type,
-- Done 2. we need to figure out the main qualified name
-- Done 3. If an haskell imporr type is use twice, we should not generate it twice.
-- 4. Add logic for TypeObject to EXT_TO in the Api section

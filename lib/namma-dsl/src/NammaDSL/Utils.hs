{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.Utils where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Lens.Combinators
import Control.Monad.Extra (concatMapM)
import Data.Aeson
import Data.Aeson.Key (fromString, fromText, toString)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Value)
import Data.Bool (bool)
import Data.Char (isLower, toLower, toUpper)
import Data.List (find, intercalate, nub)
import qualified Data.List as L
import Data.List.Extra (trim)
import Data.List.Split (split, splitOn, splitWhen, whenElt)
import qualified Data.List.Split as L
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.String.Builder (build)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Dhall (inputFile)
import Dhall.Marshal.Decode (auto)
import qualified Language.Haskell.Exts as LHE
import Language.Haskell.TH hiding (match)
import NammaDSL.Config
import NammaDSL.DSL.Syntax.API (ApiType (..), Apis (..))
import qualified NammaDSL.DSL.Syntax.API as APISyntax
import NammaDSL.DSL.Syntax.Storage (ExtraOperations (..), FieldRelation (..), Order (..), Param (..), ParamConstantType (..))
import qualified NammaDSL.GeneratorCore as GC
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO
import qualified Text.Parsec as PS
import Text.Regex.TDFA ((=~))
import Prelude

apiTypeToText :: ApiType -> Text
apiTypeToText apitype = case apitype of
  GET -> "Get"
  POST -> "Post"
  PUT -> "Put"
  DELETE -> "Delete"

startsWithLower :: String -> Bool
startsWithLower (x : _) = isLower x
startsWithLower _ = False

writeToFile :: FilePath -> FilePath -> String -> IO ()
writeToFile directoryPath fileName content = do
  createDirectoryIfMissing True directoryPath
  withFile (directoryPath ++ "/" ++ fileName) WriteMode $ \handle_ -> do
    hPutStr handle_ content

writeToFile' :: FilePath -> String -> IO ()
writeToFile' filePath content = do
  withFile filePath WriteMode $ \handle_ -> do
    hPutStr handle_ content

writeToFileIfNotExists :: FilePath -> FilePath -> String -> IO ()
writeToFileIfNotExists directoryPath fileName content = do
  exists <- doesFileExist filePath
  bool (writeToFile directoryPath fileName content) (pure ()) exists
  where
    filePath = directoryPath ++ "/" ++ fileName

typeDelimiter :: String
typeDelimiter = "() []'"

isMaybeType :: String -> Bool
isMaybeType tp = L.isPrefixOf "Maybe " tp || L.isPrefixOf "Data.Maybe.Maybe " tp || L.isPrefixOf "Kernel.Prelude.Maybe " tp

removeOccurrence :: String -> String -> String
removeOccurrence remove str = T.unpack $ T.replace (T.pack remove) (T.pack "") (T.pack str)

regexExec :: String -> String -> Maybe String
regexExec str pattern' = do
  let match = str =~ pattern' :: (String, String, String, [String])
  L.find (/= "") (snd4 match)
  where
    snd4 (_, _, _, x) = x

getFieldRelationAndHaskellType :: String -> Maybe (FieldRelation, String)
getFieldRelationAndHaskellType str' = do
  let patternOneToOne' :: String = "^Domain\\.Types\\..*\\.(.*)$"
      patternMaybeOneToOne' :: String = "^Kernel.Prelude.Maybe Domain\\.Types\\..*\\.(.*)$"
      patternOneToMany' :: String = "^\\[Domain\\.Types\\..*\\.(.*)\\]$"
      patternWithId :: String = "With(Cached){0,1}Id(Create){0,1}"
      patternWithIdCreate :: String = "With(Cached){0,1}IdCreate"
      patternFromCache :: String = "WithCachedId(Create){0,1}"
      patternNormal :: String = "NoRelation"
      needToCreate = optionalRelation =~ patternWithIdCreate
      isFromCached = optionalRelation =~ patternFromCache
  if optionalRelation =~ patternNormal
    then Nothing
    else
      if optionalRelation =~ patternWithId -- patternWithId `L.isInfixOf` optionalRelation
        then
          if "Kernel.Prelude.Maybe" `L.isPrefixOf` str
            then Just (WithId needToCreate isFromCached, getModuleNameAfterLastDot str)
            else Just (WithIdStrict needToCreate isFromCached, getModuleNameAfterLastDot str)
        else
          let fieldRelationAndHaskellType = case (regexExec str patternOneToOne', regexExec str patternMaybeOneToOne', regexExec str patternOneToMany') of
                (Just haskellType, Nothing, Nothing) -> Just (OneToOne, haskellType)
                (Nothing, Just haskellType, Nothing) -> Just (MaybeOneToOne, haskellType)
                (Nothing, Nothing, Just haskellType) -> Just (OneToMany, haskellType)
                (_, _, _) -> Nothing
           in if isJust fieldRelationAndHaskellType && validatePattern
                then fieldRelationAndHaskellType
                else Nothing
  where
    getModuleNameAfterLastDot :: String -> String
    getModuleNameAfterLastDot = last . L.splitOn "."

    (str, optionalRelation) = break (== '|') str'
    validatePattern =
      case splitOn "." str of
        [_, _, third, fourth] -> third == fourth
        _ -> False

-- makeTypeQualified (Maybe Module name)
makeTypeQualified :: [(String, String)] -> Maybe String -> Maybe [String] -> Maybe [String] -> Maybe String -> Object -> String -> String
makeTypeQualified defaultTypeImport moduleName excludedList dList defaultImportModule obj str' = concatMap replaceOrKeep (split (whenElt (`elem` typeDelimiter)) str) <> opt
  where
    (str, opt) = break (== '|') str'
    getQualifiedImport :: String -> Maybe String
    getQualifiedImport tk = case preview (ix "imports" . key (fromString tk) . _String) obj of
      Just t -> Just t
      Nothing -> find ((== tk) . fst) defaultTypeImport >>= pure . snd

    replaceOrKeep :: String -> String
    replaceOrKeep word
      | '.' `elem` word || ',' `elem` word = word
      | isJust moduleName && isJust excludedList && word `elem` fromJust excludedList = maybe "" (++ ("." ++ fromJust moduleName ++ ".")) defaultImportModule ++ word
      | isJust dList && L.elem word (fromJust dList) = maybe "" (++ ("." ++ word ++ ".")) defaultImportModule ++ word
      | otherwise = maybe (if word `elem` ["", ")", "(", " ", "[", "]", "e", "s", "'"] then word else error ("\"" ++ word ++ "\" type not determined")) (\x -> x <> "." <> word) (getQualifiedImport word)

figureOutImports :: [String] -> [String]
figureOutImports fieldTypes =
  nub $ filter (not . null) $ concatMap (map extractUptoLastDot) extractWords
  where
    extractWords = splitWhen (`elem` (typeDelimiter ++ ",")) <$> fieldTypes
    extractUptoLastDot str =
      let pp = splitOn "." str
       in if length pp > 1
            then intercalate "." (init pp)
            else
              if startsWithLower str
                then ""
                else str

-- do not use with record dot syntax
figureOutImport :: String -> Maybe String
figureOutImport str = do
  case splitOn "." str of
    (w : ws) -> Just $ intercalate "." $ init (w : ws)
    [] -> Nothing

-- Helper function to capitalize a string
capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs

-- Helper function to lowercase the first letter of a string
lowercaseFirstLetter :: String -> String
lowercaseFirstLetter "" = ""
lowercaseFirstLetter (x : xs) = toLower x : xs

_String :: Prism' Value String
_String = _Value . prism (String . T.pack) (\v -> case v of String s -> Right $ T.unpack s; _ -> Left v)

_Bool :: Prism' Value Bool
_Bool = _Value . prism Bool (\v -> case v of Bool s -> Right s; _ -> Left v)

checkArray :: Text -> (Text, Bool)
checkArray txt =
  if T.head txt == '[' && T.last txt == ']'
    then (T.init (T.tail txt), True)
    else (txt, False)

valueToString :: Value -> String
valueToString = \case
  String a -> T.unpack a
  _ -> ""

class MakeList keyType valueType where
  mkList :: Value -> [(keyType, valueType)]

instance MakeList String String where
  mkList (Object obj) =
    KM.toList obj >>= \(k, v) -> case v of
      String t -> [(toString k, T.unpack t)]
      _ -> []
  mkList (Array arr) = (concatMap mkList) $ V.toList arr
  mkList _ = []

instance MakeList Key Value where
  mkList (Object obj) = KM.toList obj
  mkList (Array arr) = (concatMap mkList) $ V.toList arr
  mkList _ = []

extraOperation :: String -> ExtraOperations
extraOperation = \case
  "EXTRA_QUERY_FILE" -> EXTRA_QUERY_FILE
  "EXTRA_DOMAIN_TYPE_FILE" -> EXTRA_DOMAIN_TYPE_FILE
  "EXTRA_CACHED_QUERY_FILE" -> EXTRA_CACHED_QUERY_FILE
  "GENERATE_INDEXES" -> GENERATE_INDEXES
  "NO_DEFAULT_INDEXES" -> NO_DEFAULT_INDEXES
  _ -> error "Invalid extra operation"

defaultOrderBy :: (String, Order)
defaultOrderBy = ("createdAt", Desc)

removeBeamFieldsWRTRelation :: Maybe FieldRelation -> Bool
removeBeamFieldsWRTRelation = \case
  Just (WithId _ _) -> True
  Just (WithIdStrict _ _) -> True
  Nothing -> True
  _ -> False

getAllJust :: [Maybe a] -> [a]
getAllJust x = fromMaybe [] $ sequence $ filter isJust x

checkParentheses :: String -> Bool
checkParentheses str = not (null str) && length str >= 2 && head str == '(' && last str == ')'

(++$) :: String -> String -> String
(++$) a b
  | checkParentheses b = a ++ " " ++ b
  | length (words b) > 1 = a ++ " $ " ++ b
  | otherwise = a ++ " " ++ b

infixr 5 ++$

isApiExtraTypesPresent :: Apis -> Bool
isApiExtraTypesPresent apiDef = not $ L.null (apiDef ^. APISyntax.apiTypes . APISyntax.types)

haskellModuleNameFromFilePath :: FilePath -> String
haskellModuleNameFromFilePath folderPath =
  intercalate "." $
    drop (maybe 0 succ ((L.elemIndex "src" pathArray) <|> (L.elemIndex "src-read-only" pathArray))) pathArray
  where
    pathArray = splitOn "/" folderPath

fetchDhallConfig :: FilePath -> IO AppConfigs
fetchDhallConfig filePath = do
  config <- inputFile auto filePath
  return config

makeAccKeysTH :: String -> Q [Dec]
makeAccKeysTH inputs = do
  let mkKey inp = do
        let keyName = mkName $ "acc_" ++ inp
            keyType = conT ''Key
            keyValue = appE (varE 'fromText) (litE $ stringL inp)
        keyDeclaration <- sigD keyName keyType
        keyDefinition <- valD (varP keyName) (normalB keyValue) []
        return [keyDeclaration, keyDefinition]
  concatMapM mkKey (nub . filter (not . null) . map trim . lines $ inputs)

getGeneratorDefaultImports :: AppConfigs -> GenerationType -> DefaultImports
getGeneratorDefaultImports config generatorTp = fromMaybe (DefaultImports [] [] [] generatorTp) $ find ((== generatorTp) . _generationType) (config ^. defaultImports)

(<||>) :: PS.ParsecT s u m a -> PS.ParsecT s u m a -> PS.ParsecT s u m a
(<||>) = (PS.<|>)

removeUnusedQualifiedImports :: GC.Code -> [String] -> [String]
removeUnusedQualifiedImports (GC.Code codeBody') qualifiedImports = (removeExplanationMark . snd) <$> filter (\(qIName, qI) -> "!" `L.isPrefixOf` qI || isQualifiedImportUsed qIName) cleanQualifiedImports
  where
    removeExplanationMark q = if "!" `L.isPrefixOf` q then drop 1 q else q
    isQualifiedImportUsed :: String -> Bool
    isQualifiedImportUsed qualifiedImportName = codeBody =~ ("[^a-zA-Z0-9.]" ++ qualifiedImportName ++ "[.]")
    codeBody = build codeBody'
    cleanQualifiedImports =
      ( \qImport ->
          ( case words qImport of
              (_ : "as" : x : _) -> trim x
              _ -> qImport,
            qImport
          )
      )
        <$> qualifiedImports

removeQuoteWrap :: String -> String
removeQuoteWrap str =
  case str of
    '\"' : rest -> case reverse rest of
      '\"' : remainder -> reverse remainder
      _ -> str
    _ -> str

isParamVariable :: Param -> Bool
isParamVariable = \case
  Constant _ _ -> False
  Variable _ _ -> True

getParamName :: Param -> String
getParamName (Constant _ _) = error "This is a constant, cant get name"
getParamName (Variable x _) = x

getVarParamType :: Param -> String
getVarParamType (Constant _ _) = error "This is a constant, cant get type"
getVarParamType (Variable _ x) = x

parseConstantParam :: String -> Param
parseConstantParam prm =
  case L.splitOn "|" prm of
    [constant, code] ->
      Constant constant (parseConstantType code)
    _ -> error "Invalid input format for constant param"

parseConstantType :: String -> ParamConstantType
parseConstantType = \case
  "CS" -> PString
  "CI" -> PInt
  "CB" -> PBool
  "CD" -> PDouble
  "CIM" -> PImportedData
  "C" -> PString
  _ -> error "Invalid Constant Type"

headToUpper :: Text -> Text
headToUpper txt = case T.uncons txt of
  Just (x, xs) -> T.cons (toUpper x) xs
  Nothing -> ""

headToLower :: Text -> Text
headToLower txt = case T.uncons txt of
  Just (x, xs) -> T.cons (toLower x) xs
  Nothing -> ""

getHaskellParsedDecs :: FilePath -> IO ([LHE.Decl LHE.SrcSpanInfo])
getHaskellParsedDecs filePath = do
  parsedHaskellFile <- LHE.parseFileWithMode parseModeConfig filePath
  return $ case parsedHaskellFile of
    LHE.ParseOk (LHE.Module _ _ _ _ decs) -> decs
    err@(LHE.ParseFailed _ _) -> error $ "Failed to parse haskell file, please report it to developers \n" <> show err
    _ -> error "Failed to parse haskell file, please report it to developers"

getHaskellFuncSigs :: [LHE.Decl LHE.SrcSpanInfo] -> [String]
getHaskellFuncSigs decs = catMaybes $ map getFuncSig decs
  where
    getFuncSig :: LHE.Decl LHE.SrcSpanInfo -> Maybe String
    getFuncSig (LHE.TypeSig _ [LHE.Ident _ funcName] _) = Just funcName
    getFuncSig _ = Nothing

parseModeConfig :: LHE.ParseMode
parseModeConfig =
  LHE.defaultParseMode
    { LHE.ignoreLanguagePragmas = True,
      LHE.ignoreFunctionArity = True,
      LHE.ignoreLinePragmas = True,
      LHE.extensions =
        filter
          ( \case
              LHE.EnableExtension _ -> True
              _ -> False
          )
          LHE.knownExtensions
    }

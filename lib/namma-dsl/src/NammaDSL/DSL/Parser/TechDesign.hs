{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module NammaDSL.DSL.Parser.TechDesign where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Lazy
import Data.Aeson
import Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (_Array, _Value)
import Data.Bool
import qualified Data.ByteString as BS
import Data.Char (isUpper)
import Data.Default
import qualified Data.List as L
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import NammaDSL.AccessorTH
import NammaDSL.DSL.Syntax.Common
import NammaDSL.DSL.Syntax.TechDesign
import NammaDSL.Utils
import System.Directory
import Prelude

parseTechDesign :: TechDM ()
parseTechDesign = do
  parseModuleMapper
  parseChangeEntities

parseModuleMapper :: TechDM ()
parseModuleMapper = do
  obj <- yamlObject <$> ask
  defaultModuleMapper <- defaultModuleMapper <$> ask
  let mdlMapper = L.nubBy (\a b -> fst a == fst b) $ defaultModuleMapper ++ (fromMaybe [] $ obj ^? ix acc_moduleMapper . _Value . to mkList)
  modify $ \s -> s {moduleMapper = mdlMapper}

parseChangeEntities :: TechDM ()
parseChangeEntities = do
  obj <- yamlObject <$> ask
  let changeList = filter (("moduleMapper" /=) . fst) (toChangeList obj)
  mapM_ parseChangeEntity changeList

parseChangeEntity :: (Text, Object) -> TechDM ()
parseChangeEntity (dName, changeObj) = do
  mdlmap <- moduleMapper <$> get
  tdPathPrefixes <- tdPathPrefixes <$> ask
  let !qDName = maybe (if '.' `T.elem` dName then dName else errorT ("Module not found for " <> dName)) (\m -> m <> "." <> dName) (lookup dName mdlmap)
      (md, accDName) = getMdAndDname qDName
      allCommentChanges = changeObj ^. ix acc_tdComments . _Array . to V.toList . to (map (mkCommentChange accDName))
      allFieldChanges = (mkList $ Object changeObj) & map \(f, v) -> AddField accDName f v
      extraChange = concatMap extraChanges allFieldChanges
  potentialFilePaths <- mapM (\pfx -> getModuleFilePath pfx (T.unpack md)) tdPathPrefixes
  let correctFilePath = fromMaybe (errorT $ "No File path found for module: " <> md) $ (listToMaybe . catMaybes) potentialFilePaths
  let annotatedChanges = map (mkAnnotated md correctFilePath) (allCommentChanges ++ allFieldChanges ++ extraChange)
  modify $ \s -> s {changes = changes s ++ annotatedChanges}

getModuleFilePath :: FilePath -> String -> TechDM (Maybe FilePath)
getModuleFilePath rootPath moduleName = do
  let partialModulePath = L.intercalate "/" (wordsBy (== '.') moduleName)
      expectedAbsFilePath = rootPath <> "/" <> partialModulePath <> ".purs"
  fileExists <- liftIO $ doesFileExist expectedAbsFilePath
  pure $ bool Nothing (Just expectedAbsFilePath) fileExists

extraChanges :: Change -> [Change]
extraChanges = \case
  (AddField _ _ tv) -> do
    let imps = filter checkIfCorrectImp $ T.pack <$> figureOutImports [T.unpack tv] -- TODO: Implement this
    (\imp -> AddImport $ PImport imp Qualified) <$> imps
  _ -> []

checkIfCorrectImp :: Text -> Bool
checkIfCorrectImp imp =
  T.splitOn "." imp
    & all
      ( \part ->
          T.uncons part & \case
            Just (h, _) -> isUpper h
            Nothing -> False
      )

getMdAndDname :: Text -> (Text, Text)
getMdAndDname qn = qn & (T.breakOnEnd ".") & \(a, b) -> (T.init a, b)

mkAnnotated :: Text -> FilePath -> Change -> Ann Change
mkAnnotated m fp c = Ann c m fp

techDesignParser :: TechDRead -> FilePath -> IO TechDesign
techDesignParser techDRead filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error "Not a Valid Yaml"
    Right yml -> evalParser parseTechDesign (techDRead {yamlObject = yml}) def

toChangeList :: Object -> [(Text, Object)]
toChangeList obj =
  KM.toList obj >>= \(k, v) -> case v of
    Object o -> [(toText k, o)]
    _ -> []

mkCommentChange :: Text -> Value -> Change
mkCommentChange dName = \case
  String a -> AddComment dName ("-- " <> a)
  _ -> errorT "Invalid Comment. Expected string"

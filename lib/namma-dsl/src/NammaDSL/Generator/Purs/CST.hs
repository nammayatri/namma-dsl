{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NammaDSL.Generator.Purs.CST (module NammaDSL.Generator.Purs.CST, module ReExport) where

import Control.Arrow ((&&&))
--import Debug.Trace (traceShowId)
import Data.Bool (bool)
import Data.Char (isDigit)
import Data.Default
import qualified Data.List as L
import Data.Maybe
import Data.String.Builder (literal)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as T
import qualified Language.PureScript.CST as P
import Language.PureScript.CST.Types
import Language.PureScript.Names (ModuleName (..), ProperName (..), runModuleName)
import Language.PureScript.PSString (mkString)
import qualified Language.PureScript.PSString as PS
import NammaDSL.DSL.Parser.TechDesign (etImp)
import NammaDSL.DSL.Syntax.TechDesign as ReExport
import NammaDSL.GeneratorCore (Code (..))
import Prelude

doCSTChanges :: FilePath -> [(Module () -> Module ())] -> IO ()
doCSTChanges pursFilePath changes = do
  contents <- T.readFile pursFilePath
  case snd (P.parse contents) of
    Left err -> print err
    Right md -> do
      let newMd = foldl (\acc f -> f acc) md changes
      T.writeFile pursFilePath (P.printModule newMd)

viewModule :: FilePath -> IO ()
viewModule pursFilePath = do
  contents <- T.readFile pursFilePath
  case snd (P.parse contents) of
    Left err -> print err
    Right md -> print md

pToken :: Text -> Token
pToken txt = TokUpperName [] txt

pExprVar :: LC -> EC -> Text -> Expr ()
pExprVar lc ec txt = ExprHole () (pName lc ec (pToken txt) (Ident txt))

pName :: LC -> EC -> Token -> a -> Name a
pName leadingComments trailingComments tok val =
  Name
    { nameTok = pSourceToken leadingComments trailingComments tok,
      nameValue = val
    }

pNameIdent :: LC -> EC -> Text -> Name Ident
pNameIdent lc ec txt = pName lc ec (pToken txt) (Ident txt)

pSourceToken :: LC -> EC -> Token -> SourceToken
pSourceToken leadingComments trailingComments tok =
  SourceToken
    { tokAnn =
        TokenAnn
          { tokRange = def,
            tokLeadingComments = leadingComments,
            tokTrailingComments = trailingComments
          },
      tokValue = tok
    }

pRecordField :: LC -> EC -> Text -> Text -> RecordLabeled (Expr ())
pRecordField lc ec fieldName fieldValue = RecordField (pLabel lc ec fieldName) (pSourceToken [] [] (TokOperator [] ":")) (pExprVar [Space 1] [] fieldValue)

addFieldToExprRecord :: Text -> Text -> Expr () -> Expr ()
addFieldToExprRecord _fieldName _fieldValue expr =
  case expr of
    ExprRecord _ann _wrp ->
      let newWrp =
            if isNothing (wrpValue _wrp)
              then _wrp {wrpValue = Just (Separated {sepHead = (pRecordField [Line LF, Space 2] [] _fieldName _fieldValue), sepTail = []})}
              else _wrp {wrpValue = (addRecordLabeledToSeperated _fieldName _fieldValue) <$> (wrpValue _wrp)}
       in ExprRecord _ann newWrp
    _exp@_ -> _exp

pTypeVar :: a -> LC -> EC -> Text -> Type a
pTypeVar ann lc ec tval = TypeVar ann (pName lc ec (TokUpperName [] tval) (Ident tval))

pTypeCons :: LC -> EC -> Text -> Type ()
pTypeCons _lc _ec = error "Lets do later if required"

pImport :: Maybe Text -> Text -> ImportDecl ()
pImport qualifiedName txt =
  ImportDecl
    { impAnn = (),
      impKeyword = pSourceToken [Line LF] [Space 1] (TokLowerName [] "import"),
      impModule = pName [] [] (TokUpperName [] txt) (ModuleName txt),
      impNames = Nothing,
      impQual =
        qualifiedName >>= \qN ->
          pure
            ( pSourceToken [Space 1] [Space 1] (TokLowerName [] "as"),
              Name {nameTok = SourceToken {tokAnn = TokenAnn {tokRange = def, tokLeadingComments = [], tokTrailingComments = []}, tokValue = TokUpperName [] qN}, nameValue = ModuleName qN}
            )
    }

pSeperated :: forall a. Text -> [a] -> Maybe (Separated a)
pSeperated sepTok items =
  let sourceToken = pSourceToken [] [] (TokLowerName [] sepTok)
      sepModuleExports = case items of
        [] -> Nothing
        (x : xs) ->
          Just $
            Separated
              { sepHead = x,
                sepTail = zip (repeat sourceToken) xs
              }
   in sepModuleExports

pWrapped :: forall a. Token -> Token -> a -> Wrapped a
pWrapped openTok endTok val =
  Wrapped
    { wrpOpen = pSourceToken [] [Space 1] openTok,
      wrpValue = val,
      wrpClose = pSourceToken [Space 1] [] endTok
    }

pExports :: [Text] -> Maybe (DelimitedNonEmpty (Export ()))
pExports exports = (pWrapped TokLeftParen TokRightParen) <$> (pSeperated "," $ map ((ExportValue ()) . (pNameIdent [] [])) exports)

pModule :: Text -> [Text] -> Module ()
pModule moduleName reExports =
  Module
    { modAnn = (),
      modKeyword = pSourceToken [] [Space 1] (TokLowerName [] "module"),
      modNamespace = pName [] [Space 1] (TokUpperName [] moduleName) (ModuleName moduleName),
      modExports = pExports reExports,
      modImports = [],
      modDecls = [],
      modTrailingComments = [],
      modWhere = pSourceToken [Space 1] [] (TokLowerName [] "where\n")
    }

addImports :: [PImport] -> Module () -> Module ()
addImports imports mod' =
  let oldImportDecls = modImports mod'
      newImportDecls = foldl (\acc imp -> addImportIfNotPresent imp acc) oldImportDecls imports
   in mod' {modImports = newImportDecls}

putImports :: [ImportDecl ()] -> Module () -> Module ()
putImports imports mod' = mod' {modImports = imports}

addImportIfNotPresent :: PImport -> [ImportDecl ()] -> [ImportDecl ()]
addImportIfNotPresent imp@(PImport val typ) _imports =
  let newImport = case typ of
        Qualified -> pImport (pure val) val
        Simple -> pImport Nothing val
   in _imports ++ [newImport | not (isPresent imp _imports)]

isPresent :: PImport -> [ImportDecl ()] -> Bool
isPresent (PImport val typ) =
  any
    ( \imp ->
        let importedModuleName = getName (impModule imp)
            qualifiedAs = getName . snd <$> impQual imp
         in importedModuleName == val && ((typ == Qualified && qualifiedAs == Just val) || (typ == Simple && isNothing (impNames imp)))
    )

findAndAddFieldToDecl :: Text -> Text -> Text -> [Declaration ()] -> [Declaration ()]
findAndAddFieldToDecl declName lblName typeName decls =
  let addFieldFunc = addFieldToDecls lblName typeName
   in findAndApply (\dec -> isDeclWithName declName dec && isDeclField dec) addFieldFunc decls

isDeclField :: Declaration a -> Bool
isDeclField = \case
  DeclValue _ _ -> True
  DeclData _ _ _ -> False -- For now will add support later
  DeclType _ _ _ _ -> True
  DeclNewtype _ _ _ _ _ -> True
  _ -> False

isDeclSignature :: Declaration a -> Bool
isDeclSignature = \case
  DeclSignature _ _ -> True
  _ -> False

pDeclSignature :: Text -> Text -> Declaration ()
pDeclSignature declName typeName =
  DeclSignature
    ()
    ( Labeled
        { lblLabel = pNameIdent [Line LF, Line LF] [] declName,
          lblSep = pSourceToken [Space 1] [] (TokDoubleColon ASCII),
          lblValue = pTypeVar () [Space 1] [] typeName
        }
    )

pTypeWithArrow :: Text -> [Text] -> (Text, [ImportDecl ()])
pTypeWithArrow extra types =
  let reqImports = map (\imp -> pImport (Just imp) imp) (T.pack <$> (etImp $ T.unpack <$> types))
      arrowedType = T.intercalate " -> " types
      typeExtraWithArrowed = extra <> " " <> arrowedType
   in (typeExtraWithArrowed, reqImports)

pToBeImplementedFunction :: Text -> Text -> Text -> Module () -> Module ()
pToBeImplementedFunction declName typeName defaultExpr md =
  md
    { modDecls =
        modDecls md
          ++ [ pDeclSignature declName typeName,
               DeclValue
                 ()
                 ( ValueBindingFields
                     { valName = pNameIdent [Line LF] [] declName,
                       valBinders = [],
                       valGuarded = Unconditional (pSourceToken [Space 1] [Space 1] (TokEquals)) (Where {whereExpr = pExprVar [] [] defaultExpr, whereBindings = Nothing})
                     }
                 )
             ]
    }

isDeclWithName :: Text -> Declaration a -> Bool
isDeclWithName searchName decl = getName decl == searchName

findDeclWithName :: Text -> [Declaration a] -> Maybe (Declaration a)
findDeclWithName searchName decls = L.find (isDeclWithName searchName) decls

addLabelToRow :: Labeled Label (Type a) -> Row a -> Row a
addLabelToRow lbl row =
  let newRL =
        case (rowLabels row) of
          sep@(Just _) -> addLabelToSeperated lbl <$> sep
          Nothing -> Just (Separated {sepHead = cmtUp [Line LF, Space 4] lbl, sepTail = []})
   in row {rowLabels = newRL}

addLabelToSeperated :: Labeled Label (Type a) -> Separated (Labeled Label (Type a)) -> Separated (Labeled Label (Type a))
addLabelToSeperated lbl sep = sep {sepTail = sepTail sep ++ [fieldWithComma]}
  where
    fieldWithComma = (pSourceToken [] [] TokComma, lbl)

addRecordLabeledToSeperated :: Text -> Text -> Separated (RecordLabeled (Expr ())) -> Separated (RecordLabeled (Expr ()))
addRecordLabeledToSeperated fieldName fieldValue sep = sep {sepTail = sepTail sep ++ [fieldWithComma]}
  where
    (lc, ec) = (getLC &&& getEC) sep
    lbl = pRecordField lc ec fieldName fieldValue
    fieldWithComma = (pSourceToken [] [] TokComma, lbl)

addFieldToDecls :: Text -> Text -> Declaration () -> Declaration ()
addFieldToDecls lblName typeName decl =
  bool
    decl
    ( case decl of
        DeclNewtype ann dh s nm tp ->
          let (lc, ec) =
                ( \case
                    TypeRecord _ wrp -> (getLC &&& getEC) wrp
                    TypeRow _ wrp -> (getLC &&& getEC) wrp
                    _ -> ([], [])
                )
                  tp
              labeled = pLabeled lc ec lblName (pTypeVar ann [Space 1] [] typeName)
           in DeclNewtype ann dh s nm (addLabelToTypeRecord labeled tp) -- TODO: For other Types it will break.. only for DeclNewtype
        DeclType ann dh s tp ->
          let (lc, ec) =
                ( \case
                    TypeRecord _ wrp -> (getLC &&& getEC) wrp
                    TypeRow _ wrp -> (getLC &&& getEC) wrp
                    _ -> ([], [])
                )
                  tp
              labeled = pLabeled lc ec lblName (pTypeVar ann [Space 1] [] typeName)
           in DeclType ann dh s (addLabelToTypeRecord labeled tp) -- TODO: For other Types it will break.. only for DeclType
        DeclValue ann vbf ->
          DeclValue ann (vbf {valGuarded = newValGuarded})
          where
            newValGuarded = case valGuarded vbf of
              Unconditional st whereA ->
                let newWhere = whereA {whereExpr = addFieldToExprRecord lblName typeName (whereExpr whereA)}
                 in Unconditional st newWhere
              _ -> valGuarded vbf
        dec@_ -> dec
    )
    (lblName `notElem` getFields decl)

addLabelToTypeRecord :: Labeled Label (Type a) -> Type a -> Type a
addLabelToTypeRecord lbl = \case
  TypeRecord ann wrpRows -> TypeRecord ann (wrpRows {wrpValue = addLabelToRow lbl (wrpValue wrpRows)})
  TypeRow ann wrpRows -> TypeRow ann (wrpRows {wrpValue = addLabelToRow lbl (wrpValue wrpRows)})
  ty@_ -> ty

pLabeled :: LC -> EC -> Text -> Type a -> Labeled Label (Type a)
pLabeled lc _ec lblName typ =
  Labeled
    { lblLabel = pLabel lc [Space 1] lblName,
      lblSep = pSourceToken [] [] (TokDoubleColon ASCII),
      lblValue = typ
    }

-- TODO: remember we need to extrart the LCs from previous fields
-- to match the indentation
pLabel :: LC -> EC -> Text -> Label
pLabel lc ec lblName =
  Label
    { lblTok = pSourceToken lc ec (TokUpperName mempty lblName),
      lblName = mkString lblName
    }

findAndApply :: (a -> Bool) -> (a -> a) -> [a] -> [a]
findAndApply _ _ [] = []
findAndApply isElementCondition functionTobeApplied (x : xs)
  | isElementCondition x = functionTobeApplied x : xs
  | otherwise = x : findAndApply isElementCondition functionTobeApplied xs

class GetCLF a where
  getLC :: a -> LC
  getEC :: a -> EC

instance GetCLF (Declaration a) where
  getLC = \case
    DeclSignature _ lbled -> getLC lbled
    _ -> []
  getEC = \case
    DeclSignature _ lbled -> getEC lbled
    _ -> []

instance GetCLF (Name a) where
  getLC = getLC . nameTok
  getEC = getEC . nameTok

instance GetCLF SourceToken where
  getLC = tokLeadingComments . tokAnn
  getEC = tokTrailingComments . tokAnn

instance GetCLF () where
  getLC _ = []
  getEC _ = []

instance GetCLF Label where
  getLC = getLC . lblTok
  getEC = getEC . lblTok

instance GetCLF a => GetCLF (Labeled a b) where
  getLC = getLC . lblLabel
  getEC = getEC . lblLabel

instance GetCLF a => GetCLF (Separated a) where
  getLC = getLC . sepHead
  getEC = getEC . sepHead

instance GetCLF a => GetCLF (Row a) where
  getLC = (maybe [] getLC) . rowLabels
  getEC = (maybe [] getEC) . rowLabels

instance GetCLF a => GetCLF (Wrapped a) where
  getLC = getLC . wrpValue
  getEC = getEC . wrpValue

instance GetCLF a => GetCLF (Maybe a) where
  getLC = maybe [] getLC
  getEC = maybe [] getEC

-- check later --
instance GetCLF a => GetCLF (Expr a) where
  getLC _ = []
  getEC _ = []

instance GetCLF a => GetCLF (RecordLabeled a) where
  getLC recL = case recL of
    RecordField lbl _ _ -> getLC lbl
    RecordPun _ -> mempty
  getEC recL = case recL of
    RecordField lbl _ _ -> getEC lbl
    RecordPun _ -> mempty

class GetName a where
  getName :: a -> Text

instance GetName (Declaration a) where
  getName = \case
    DeclData _ dh _ -> getName dh
    DeclNewtype _ dh _ _ _ -> getName dh
    DeclType _ dh _ _ -> getName dh
    DeclValue _ vbf -> getName vbf
    DeclSignature _ lbled -> getName lbled
    _ -> "So many will check later if required"

instance GetName a => GetName (Labeled a b) where
  getName = getName . lblLabel

instance GetName (ValueBindingFields a) where
  getName = getName . valName

instance GetName Ident where
  getName = getIdent

instance GetName (DataHead a) where
  getName = getName . dataHdName

instance GetName a => GetName (Name a) where
  getName = getName . nameValue

instance GetName (ProperName k) where
  getName = runProperName

instance GetName (ModuleName) where
  getName = runModuleName

instance Default SourceRange where
  def = SourceRange (SourcePos 0 0) (SourcePos 0 0)

class CmtUp a where
  cmtUp :: LC -> a -> a
  cmtUpStrict :: LC -> a -> a

instance CmtUp Label where
  cmtUp cmt lbl = lbl {lblTok = cmtUp cmt (lblTok lbl)}
  cmtUpStrict cmt lbl = lbl {lblTok = cmtUpStrict cmt (lblTok lbl)}

instance CmtUp TokenAnn where
  cmtUp cmt tokAnn = tokAnn {tokLeadingComments = (tokLeadingComments tokAnn) ++ cmt}
  cmtUpStrict cmt tokAnn = tokAnn {tokLeadingComments = cmt}

instance CmtUp SourceToken where
  cmtUp cmt srcTok = srcTok {tokAnn = cmtUp cmt (tokAnn srcTok)}
  cmtUpStrict cmt srcTok = srcTok {tokAnn = cmtUpStrict cmt (tokAnn srcTok)}

instance CmtUp (Name a) where
  cmtUp cmt nm = nm {nameTok = cmtUp cmt (nameTok nm)}
  cmtUpStrict cmt nm = nm {nameTok = cmtUpStrict cmt (nameTok nm)}

instance CmtUp a => CmtUp (Labeled a b) where
  cmtUp cmt lbl = lbl {lblLabel = cmtUp cmt (lblLabel lbl)}
  cmtUpStrict cmt lbl = lbl {lblLabel = cmtUpStrict cmt (lblLabel lbl)}

instance CmtUp (Declaration a) where
  cmtUp cmt = \case
    DeclSignature ann lbled -> DeclSignature ann (cmtUp cmt lbled)
    decl@_ -> decl
  cmtUpStrict cmt = \case
    DeclSignature ann lbled -> DeclSignature ann (cmtUpStrict cmt lbled)
    decl@_ -> decl

class GetFields a where
  getFields :: a -> [Text]

instance GetFields Label where
  getFields lbl = [fromMaybe mempty (PS.decodeString $ lblName lbl)]

instance GetFields a => GetFields (Labeled a b) where
  getFields = getFields . lblLabel

instance GetFields a => GetFields (Separated a) where
  getFields sep = getFields (sepHead sep) ++ concatMap getFields (snd <$> sepTail sep)

instance GetFields a => GetFields (Maybe a) where
  getFields = maybe [] getFields

instance GetFields (Row a) where
  getFields = maybe [] getFields . rowLabels

instance GetFields a => GetFields (Wrapped a) where
  getFields = getFields . wrpValue

instance GetFields (Type a) where
  getFields = \case
    TypeRecord _ wrp -> getFields wrp
    TypeRow _ wrp -> getFields wrp
    _ -> []

instance GetFields (RecordLabeled a) where
  getFields = \case
    RecordField lbl _ _ -> getFields lbl
    RecordPun _ -> []

instance GetFields a => GetFields (Name a) where
  getFields = getFields . nameValue

instance GetFields Ident where
  getFields = pure . getIdent

instance GetFields (Expr a) where
  getFields = \case
    ExprHole _ nm -> getFields nm
    ExprRecord _ wrp -> getFields wrp
    _ -> []

instance GetFields (Where a) where
  getFields = getFields . whereExpr

instance GetFields (ValueBindingFields a) where
  getFields = getFields . valGuarded

instance GetFields (Guarded a) where
  getFields = \case
    Unconditional _ whr -> getFields whr
    _ -> []

instance GetFields (Declaration a) where
  getFields = \case
    DeclNewtype _ _ _ _ tp -> getFields tp
    DeclType _ _ _ tp -> getFields tp
    DeclValue _ vbf -> getFields vbf
    _ -> []

addCmtUpDeclSig :: Text -> Comment LineFeed -> [Declaration ()] -> [Declaration ()]
addCmtUpDeclSig searchName cmt decls = findAndApply (\dec -> isDeclWithName searchName dec && isDeclSignature dec) (addComment cmt) decls

addComment :: Comment LineFeed -> Declaration () -> Declaration ()
addComment cmt decl =
  let existingCmts = getLC decl
      newCmts = updateOrAddComment cmt existingCmts
   in cmtUpStrict newCmts decl

parseComment :: Comment LineFeed -> Maybe (Int, Text)
parseComment (Comment txt) = do
  let (idPart, rest) = T.breakOn " " (T.drop 2 txt)
  if not (T.null idPart) && T.all isDigit idPart && not (T.null rest)
    then Just (read (T.unpack idPart), T.drop 1 rest)
    else Nothing
parseComment _ = Nothing

updateOrAddComment :: Comment LineFeed -> [Comment LineFeed] -> [Comment LineFeed]
updateOrAddComment newCmt oldCmts =
  case parseComment newCmt of
    Just (newId, newContent) ->
      let (found, updatedCmts) =
            foldl
              ( \(finalCheck, allCmts) oldC ->
                  let (chk, upC) = updateComment newId newContent oldC
                   in (chk || finalCheck, allCmts ++ [upC])
              )
              (False, [])
              oldCmts
       in if found then updatedCmts else oldCmts ++ [newCmt, Line LF]
    Nothing -> oldCmts -- This case should not happen as new comment is always correctly formatted

updateComment :: Int -> Text -> Comment LineFeed -> (Bool, Comment LineFeed)
updateComment newId newContent oldCmt =
  case parseComment oldCmt of
    Just (oldId, _) -> if oldId == newId then (True, Comment ("--" <> T.pack (show newId) <> " " <> newContent)) else (False, oldCmt)
    _ -> (False, oldCmt)

isCmtRequired :: Declaration () -> [Comment LineFeed] -> Bool
isCmtRequired decl cmts = not $ any (cmtAlreadyPresent decl) cmts

cmtAlreadyPresent :: Declaration () -> Comment LineFeed -> Bool
cmtAlreadyPresent decl cmt = case cmt of
  c@(Comment _) -> c `elem` (getLC decl)
  _ -> False

-- making new data types --
instance Default (Wrapped (Row a)) where
  def = Wrapped {wrpOpen = pSourceToken [] [] TokLeftBrace, wrpValue = Row {rowLabels = Nothing, rowTail = Nothing}, wrpClose = pSourceToken [] [] TokRightBrace}

instance Default (Type ()) where
  def = TypeRecord () def

pDataHead :: PRecordType -> RecordName -> DataHead ()
pDataHead recType recName =
  DataHead
    { dataHdKeyword =
        pSourceToken
          [Line LF, Line LF]
          [Space 1]
          ( TokLowerName
              []
              ( case recType of
                  PNEWTYPE -> "newtype"
                  PTYPE -> "type"
              )
          ),
      dataHdName = pName [] [Space 1] (pToken recName) (ProperName recName),
      dataHdVars = []
    }

addNewDecl :: PRecordType -> RecordName -> Declaration ()
addNewDecl recType recName =
  let dh = pDataHead recType recName
      eqTok = pSourceToken [] [Space 1] TokEquals
      consName = pName [] [Space 1] (pToken recName) (ProperName recName)
   in case recType of
        PNEWTYPE -> DeclNewtype () dh eqTok consName def
        PTYPE -> DeclType () dh eqTok def

moduleToCode :: Module () -> Code
moduleToCode md = Code (literal . T.unpack $ P.printModule md)

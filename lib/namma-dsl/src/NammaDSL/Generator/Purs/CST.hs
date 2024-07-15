{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NammaDSL.Generator.Purs.CST where

import Control.Arrow ((&&&))
import Data.Default
import qualified Data.List as L
import Data.Text (Text)
import Data.Text.IO as T
import Data.Void (Void)
import qualified Language.PureScript.CST as P
import Language.PureScript.CST.Types
import Language.PureScript.Names (ModuleName (..), ProperName (..))
import Language.PureScript.PSString (mkString)
import Prelude

type LC = [Comment LineFeed]

type EC = [Comment Void]

-- Notes:
-- 1. ProperName has several kinds. Check later
-- 2. trailing comments cant have line feed.

doCSTChanges :: FilePath -> [(Module () -> Module ())] -> IO ()
doCSTChanges pursFilePath changes = do
  contents <- T.readFile pursFilePath
  case snd (P.parse contents) of
    Left err -> print err
    Right md -> do
      let newMd = foldl (\acc f -> f acc) md changes
      T.writeFile pursFilePath (P.printModule newMd)

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
      let newWrp = _wrp {wrpValue = (addRecordLabeledToSeperated _fieldName _fieldValue) <$> (wrpValue _wrp)}
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

addImport :: Module () -> ImportDecl () -> Module ()
addImport mod' importDecl = mod' {modImports = modImports mod' ++ [importDecl]}

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

isDeclWithName :: Text -> Declaration a -> Bool
isDeclWithName searchName decl = getName decl == searchName

findDeclWithName :: Text -> [Declaration a] -> Maybe (Declaration a)
findDeclWithName searchName decls = L.find (isDeclWithName searchName) decls

addLabelToRow :: Labeled Label (Type a) -> Row a -> Row a
addLabelToRow lbl row = row {rowLabels = addLabelToSeperated lbl <$> (rowLabels row)}

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
  case decl of
    DeclNewtype ann dh s nm tp ->
      let (lc, ec) =
            ( \case
                (TypeRecord _ wrp) -> (getLC &&& getEC) wrp
                _ -> ([], [])
            )
              tp
          labeled = pLabeled lc ec lblName (pTypeVar ann [Space 1] [] typeName)
       in DeclNewtype ann dh s nm (addLabelToTypeRecord labeled tp) -- TODO: For other Types it will break.. only for DeclNewtype
    DeclType ann dh s tp ->
      let (lc, ec) =
            ( \case
                (TypeRecord _ wrp) -> (getLC &&& getEC) wrp
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

addLabelToTypeRecord :: Labeled Label (Type a) -> Type a -> Type a
addLabelToTypeRecord lbl (TypeRecord ann wrpRows) = TypeRecord ann (wrpRows {wrpValue = addLabelToRow lbl (wrpValue wrpRows)})
addLabelToTypeRecord _ _ = error "Not Implemented for other Type"

pLabeled :: LC -> EC -> Text -> Type a -> Labeled Label (Type a)
pLabeled lc ec lblName typ =
  Labeled
    { lblLabel = pLabel lc ec lblName,
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

instance Default SourceRange where
  def = SourceRange (SourcePos 0 0) (SourcePos 0 0)

class CmtUp a where
  cmtUp :: LC -> a -> a

instance CmtUp TokenAnn where
  cmtUp cmt tokAnn = tokAnn {tokLeadingComments = (tokLeadingComments tokAnn) ++ cmt}

instance CmtUp SourceToken where
  cmtUp cmt srcTok = srcTok {tokAnn = cmtUp cmt (tokAnn srcTok)}

instance CmtUp (Name a) where
  cmtUp cmt nm = nm {nameTok = cmtUp cmt (nameTok nm)}

instance CmtUp a => CmtUp (Labeled a b) where
  cmtUp cmt lbl = lbl {lblLabel = cmtUp cmt (lblLabel lbl)}

instance CmtUp (Declaration a) where
  cmtUp cmt = \case
    DeclSignature ann lbled -> DeclSignature ann (cmtUp cmt lbled)
    decl@_ -> decl

addCmtUpDeclSig :: Text -> LC -> [Declaration ()] -> [Declaration ()]
addCmtUpDeclSig searchName cmt decls = findAndApply (\dec -> isDeclWithName searchName dec && isDeclSignature dec) (cmtUp cmt) decls

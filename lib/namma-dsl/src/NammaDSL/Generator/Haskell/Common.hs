module NammaDSL.Generator.Haskell.Common where

import Data.Map (Map, lookup)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common
import NammaDSL.Lib
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import Prelude hiding (lookup)

_Maybe :: TH.Q r TH.Type
_Maybe = cT "Kernel.Prelude.Maybe"

_Id :: TH.Q r TH.Type
_Id = cT "Kernel.Types.Id.Id"

_Person :: TH.Q r TH.Type
_Person = cT "Domain.Types.Person.Person"

_Merchant :: TH.Q r TH.Type
_Merchant = cT "Domain.Types.Merchant.Merchant"

-- TODO: These should n't be hardcoded ..
_MerchantOperatingCity :: TH.Q r TH.Type
_MerchantOperatingCity = cT "Domain.Types.MerchantOperatingCity.MerchantOperatingCity"

apiAuthTypeMapperDomainHandler :: ApiTT -> Maybe (TH.Q r TH.Type)
apiAuthTypeMapperDomainHandler apiT = case _authType apiT of
  Just (DashboardAuth _) -> Just $ cT "TokenInfo"
  Just NoAuth -> Nothing
  Just (SafetyWebhookAuth _) -> Just $ cT "AuthToken"
  Just (TokenAuth tp) -> case tp of
    RIDER_TYPE -> Just $ tupleT 2 ~~ (_Maybe ~~ (_Id ~~ _Person)) ~~ (_Id ~~ _Merchant)
    PROVIDER_TYPE -> Just $ tupleT 3 ~~ (_Maybe ~~ (_Id ~~ _Person)) ~~ (_Id ~~ _Merchant) ~~ (_Id ~~ _MerchantOperatingCity)
  _ -> Just $ tupleT 2 ~~ (_Maybe ~~ (_Id ~~ _Person)) ~~ (_Id ~~ _Merchant)

apiAuthTypeMapperServant :: ApiTT -> Maybe (TH.Q r TH.Type)
apiAuthTypeMapperServant apiT = case _authType apiT of
  Just (DashboardAuth _) -> Just $ cT "TokenInfo"
  Just (SafetyWebhookAuth _) -> Just $ cT "AuthToken"
  Just NoAuth -> Nothing
  Just (TokenAuth tp) -> case tp of
    RIDER_TYPE -> Just $ tupleT 2 ~~ (_Id ~~ _Person) ~~ (_Id ~~ _Merchant)
    PROVIDER_TYPE -> Just $ tupleT 3 ~~ (_Id ~~ _Person) ~~ (_Id ~~ _Merchant) ~~ (_Id ~~ _MerchantOperatingCity)
  _ -> Just $ tupleT 2 ~~ (_Id ~~ _Person) ~~ (_Id ~~ _Merchant)

getRecordType :: RecordType -> String
getRecordType = \case
  NewType -> "newtype"
  Data -> "data"
  Type -> "type"

checkForPackageOverrides :: forall a. (Importable a, Eq a, Ord a, Semigroup a, IsString a) => Map a a -> [a] -> [a]
checkForPackageOverrides packageOverrides = map (\x -> maybe x (\a -> "\"" <> a <> "\" " <> x) (lookup (getImportSignature x) packageOverrides))

class Importable a where
  getImportSignature :: a -> a

instance Importable Text where
  getImportSignature = head . T.words

instance Importable String where
  getImportSignature = head . words

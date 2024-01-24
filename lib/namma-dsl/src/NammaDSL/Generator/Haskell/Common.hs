module NammaDSL.Generator.Haskell.Common where

import Data.Map (Map, lookup)
import qualified Data.Text as T
import Kernel.Prelude hiding (lookup, replicateM)
import NammaDSL.DSL.Syntax.API

apiAuthTypeMapperDomainHandler :: ApiTT -> Maybe Text
apiAuthTypeMapperDomainHandler apiT = case _authType apiT of
  Just (DashboardAuth _) -> Just "TokenInfo"
  Just NoAuth -> Nothing
  Just (TokenAuth tp) -> case tp of
    RIDER_TYPE -> Just "(Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)"
    PROVIDER_TYPE -> Just "(Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity)"
  _ -> Just "(Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)"

apiAuthTypeMapperServant :: ApiTT -> Maybe Text
apiAuthTypeMapperServant apiT = case _authType apiT of
  Just (DashboardAuth _) -> Just "TokenInfo"
  Just NoAuth -> Nothing
  Just (TokenAuth tp) -> case tp of
    RIDER_TYPE -> Just "(Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)"
    PROVIDER_TYPE -> Just "(Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity)"
  _ -> Just "(Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)"

checkForPackageOverrides :: forall a. (Importable a, Eq a, Ord a, Semigroup a, IsString a) => Map a a -> [a] -> [a]
checkForPackageOverrides packageOverrides = map (\x -> maybe x (\a -> "\"" <> a <> "\" " <> x) (lookup (getImportSignature x) packageOverrides))

class Importable a where
  getImportSignature :: a -> a

instance Importable Text where
  getImportSignature = head . T.words

instance Importable String where
  getImportSignature = head . words

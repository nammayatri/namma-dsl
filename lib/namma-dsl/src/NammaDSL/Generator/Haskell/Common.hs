module NammaDSL.Generator.Haskell.Common where

import Kernel.Prelude hiding (replicateM)
import NammaDSL.DSL.Syntax.API

apiAuthTypeMapper :: ApiTT -> Maybe Text
apiAuthTypeMapper apiT = case _authType apiT of
  Just (DashboardAuth _) -> Just "TokenInfo"
  Just NoAuth -> Nothing
  _ -> Just "(Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)"

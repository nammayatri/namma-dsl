module API.Instances.Reels where

import Data.Maybe
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (decodeJSON)
import Prelude
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorResponse, Method(..), defaultMakeRequest, standardEncode)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Services.Config (getBaseUrl)

newtype Something = Something {
  somethingA :: Prim.Tuple String Int,
  somethingB :: Maybe (Prim.Array String)
 }

newtype ReelsResp = ReelsResp {
  ggg :: Prim.Array (Prim.Array (Prim.Tuple (Maybe Number) Int)),
  reels :: Prim.Array A.Bls,
  rrr :: Something
 }


newtype GetReelsGetAllReelVideosRequest = GetReelsGetAllReelVideosRequest {}

newtype ReelsResp = ReelsResp {}

newtype GetReelsGetAllReelVideosRequestFull = GetReelsGetAllReelVideosRequestFull String String

instance makeGetReelsGetAllReelVideosRequest :: RestEndpoint GetReelsGetAllReelVideosRequestFull ReelsResp where
  makeRequest reqBody@(GetReelsGetAllReelVideosRequestFull language reelsKey _) headers = do
    let url = (getBaseUrl "") <> "/reels" <> "/getAllReelVideos" <> "?language=" <> language <> "&reelsKey=" <> reelsKey
    defaultMakeRequest GET url headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req

derive instance genericGetReelsGetAllReelVideosRequest :: Generic GetReelsGetAllReelVideosRequest _
derive instance newtypeGetReelsGetAllReelVideosRequest :: Newtype GetReelsGetAllReelVideosRequest _
instance standardEncodeGetReelsGetAllReelVideosRequest :: StandardEncode GetReelsGetAllReelVideosRequest where standardEncode (GetReelsGetAllReelVideosRequest body) = standardEncode body
instance showGetReelsGetAllReelVideosRequest :: Show GetReelsGetAllReelVideosRequest where show = genericShow
instance decodeGetReelsGetAllReelVideosRequest :: Decode GetReelsGetAllReelVideosRequest where decode = defaultDecode
instance encodeGetReelsGetAllReelVideosRequest :: Encode GetReelsGetAllReelVideosRequest where encode = defaultEncode

derive instance genericReelsResp :: Generic ReelsResp _
derive instance newtypeReelsResp :: Newtype ReelsResp _
instance standardEncodeReelsResp :: StandardEncode ReelsResp where standardEncode (ReelsResp body) = standardEncode body
instance showReelsResp :: Show ReelsResp where show = genericShow
instance decodeReelsResp :: Decode ReelsResp where decode = defaultDecode
instance encodeReelsResp :: Encode ReelsResp where encode = defaultEncode

derive instance genericGetReelsGetAllReelVideosRequestFull :: Generic GetReelsGetAllReelVideosRequestFull _
derive instance newtypeGetReelsGetAllReelVideosRequestFull :: Newtype GetReelsGetAllReelVideosRequestFull _
instance standardEncodeGetReelsGetAllReelVideosRequestFull :: StandardEncode GetReelsGetAllReelVideosRequestFull where standardEncode (GetReelsGetAllReelVideosRequestFull _ _ body) = standardEncode body
instance showGetReelsGetAllReelVideosRequestFull :: Show GetReelsGetAllReelVideosRequestFull where show = genericShow
instance decodeGetReelsGetAllReelVideosRequestFull :: Decode GetReelsGetAllReelVideosRequestFull where decode = defaultDecode
instance encodeGetReelsGetAllReelVideosRequestFull :: Encode GetReelsGetAllReelVideosRequestFull where encode = defaultEncode


------------------------
newtype GetReelsGetAllReelVideosForMerchantRequest = GetReelsGetAllReelVideosForMerchantRequest {}

newtype ReelsResp = ReelsResp {}

newtype GetReelsGetAllReelVideosForMerchantRequestFull = GetReelsGetAllReelVideosForMerchantRequestFull String String String

instance makeGetReelsGetAllReelVideosForMerchantRequest :: RestEndpoint GetReelsGetAllReelVideosForMerchantRequestFull ReelsResp where
  makeRequest reqBody@(GetReelsGetAllReelVideosForMerchantRequestFull language merchantId reelsKey _) headers = do
    let url = (getBaseUrl "") <> "/reels" <> "/getAllReelVideosForMerchant" <> "?language=" <> language <> "&merchantId=" <> merchantId <> "&reelsKey=" <> reelsKey
    defaultMakeRequest GET url headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req

derive instance genericGetReelsGetAllReelVideosForMerchantRequest :: Generic GetReelsGetAllReelVideosForMerchantRequest _
derive instance newtypeGetReelsGetAllReelVideosForMerchantRequest :: Newtype GetReelsGetAllReelVideosForMerchantRequest _
instance standardEncodeGetReelsGetAllReelVideosForMerchantRequest :: StandardEncode GetReelsGetAllReelVideosForMerchantRequest where standardEncode (GetReelsGetAllReelVideosForMerchantRequest body) = standardEncode body
instance showGetReelsGetAllReelVideosForMerchantRequest :: Show GetReelsGetAllReelVideosForMerchantRequest where show = genericShow
instance decodeGetReelsGetAllReelVideosForMerchantRequest :: Decode GetReelsGetAllReelVideosForMerchantRequest where decode = defaultDecode
instance encodeGetReelsGetAllReelVideosForMerchantRequest :: Encode GetReelsGetAllReelVideosForMerchantRequest where encode = defaultEncode

derive instance genericReelsResp :: Generic ReelsResp _
derive instance newtypeReelsResp :: Newtype ReelsResp _
instance standardEncodeReelsResp :: StandardEncode ReelsResp where standardEncode (ReelsResp body) = standardEncode body
instance showReelsResp :: Show ReelsResp where show = genericShow
instance decodeReelsResp :: Decode ReelsResp where decode = defaultDecode
instance encodeReelsResp :: Encode ReelsResp where encode = defaultEncode

derive instance genericGetReelsGetAllReelVideosForMerchantRequestFull :: Generic GetReelsGetAllReelVideosForMerchantRequestFull _
derive instance newtypeGetReelsGetAllReelVideosForMerchantRequestFull :: Newtype GetReelsGetAllReelVideosForMerchantRequestFull _
instance standardEncodeGetReelsGetAllReelVideosForMerchantRequestFull :: StandardEncode GetReelsGetAllReelVideosForMerchantRequestFull where standardEncode (GetReelsGetAllReelVideosForMerchantRequestFull _ _ _ body) = standardEncode body
instance showGetReelsGetAllReelVideosForMerchantRequestFull :: Show GetReelsGetAllReelVideosForMerchantRequestFull where show = genericShow
instance decodeGetReelsGetAllReelVideosForMerchantRequestFull :: Decode GetReelsGetAllReelVideosForMerchantRequestFull where decode = defaultDecode
instance encodeGetReelsGetAllReelVideosForMerchantRequestFull :: Encode GetReelsGetAllReelVideosForMerchantRequestFull where encode = defaultEncode


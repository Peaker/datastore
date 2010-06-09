{-# OPTIONS -O2 -Wall #-}

module ByteStringUtils
    (decodeS, encodeS, lazifyBS, strictifyBS)
where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary as Binary
import Data.Binary(Binary)

strictifyBS :: LBS.ByteString -> SBS.ByteString
strictifyBS = SBS.concat . LBS.toChunks

lazifyBS :: SBS.ByteString -> LBS.ByteString
lazifyBS = LBS.fromChunks . return

decodeS :: Binary a => SBS.ByteString -> a
decodeS = Binary.decode . lazifyBS

encodeS :: Binary a => a -> SBS.ByteString
encodeS = strictifyBS . Binary.encode

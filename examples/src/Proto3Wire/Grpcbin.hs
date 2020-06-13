{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Proto3Wire.Grpcbin where

import Data.Text.Lazy (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Network.GRPC.HTTP2.Proto3Wire (Proto3WireEncoder(..))
import qualified Proto3.Wire.Encode as PBEnc
import qualified Proto3.Wire.Decode as PBDec

data EmptyMessage = EmptyMessage

data Endpoint = Endpoint {
    path :: !Text
  , description :: !Text
  } deriving Show

data IndexReply = IndexReply {
    desc :: !Text
  , endpoints :: !(Seq Endpoint) 
  } deriving Show

instance Proto3WireEncoder EmptyMessage where
  proto3WireEncode :: EmptyMessage -> PBEnc.MessageBuilder
  proto3WireEncode = mempty

  proto3WireDecode :: PBDec.Parser PBDec.RawMessage EmptyMessage
  proto3WireDecode = error "todo decode emptymessage, not used in this example"

instance Proto3WireEncoder IndexReply where
  proto3WireEncode :: IndexReply -> PBEnc.MessageBuilder
  proto3WireEncode = error "todo encode indexreply, not used in this example"

  proto3WireDecode :: PBDec.Parser PBDec.RawMessage IndexReply
  proto3WireDecode =
    IndexReply
    <$> (PBDec.one PBDec.text mempty `PBDec.at` 1)
    <*> (fmap Seq.fromList (PBDec.repeated (PBDec.embedded' parseEndpoint) `PBDec.at` 2))

    where 
      parseEndpoint =
          Endpoint
          <$> (PBDec.one PBDec.text mempty `PBDec.at` 1)
          <*> (PBDec.one PBDec.text mempty `PBDec.at` 2)

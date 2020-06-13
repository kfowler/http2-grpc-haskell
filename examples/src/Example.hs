{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Example where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Network.GRPC.Client (open, singleRequest, Timeout(..), RawReply)
import Network.HTTP2.Client
import Network.GRPC.Client.Helpers
import Network.GRPC.HTTP2.Encoding

-- proto-lens
import Lens.Micro
import qualified Network.GRPC.HTTP2.ProtoLens as ProtoLens
import Data.ProtoLens
import Proto.Protos.Grpcbin (GRPCBin)
import Proto.Protos.Grpcbin_Fields (reason, code)

-- proto3-wire
import Proto3Wire.Grpcbin (EmptyMessage(..), IndexReply)
import qualified Network.GRPC.HTTP2.Proto3Wire as Proto3Wire

main :: IO ()
main = do
  let encoding = Encoding uncompressed
  let decoding = Decoding uncompressed
  let host = "grpcb.in"
  let port = 9000
  void $ runClientIO $ do
    conn <- newHttp2FrameConnection host port (tlsSettings False host port)
    runHttp2Client conn 8192 8192 [] defaultGoAwayHandler ignoreFallbackHandler $ \client -> do
      liftIO $ putStrLn "~~~connected~~~"
      let ifc = _incomingFlowControl client
      let ofc = _outgoingFlowControl client
      liftIO $ _addCredit ifc 10000000
      _ <- _updateWindow ifc

      liftIO $ putStrLn "~~~showing off proto-lens~~~"
      reply <- open client "grpcb.in:9000" [] (Timeout 100) encoding decoding
        (singleRequest (ProtoLens.RPC :: ProtoLens.RPC GRPCBin "index") defMessage)
      liftIO $ print reply

      liftIO $ putStrLn "~~~showing off proto3-wire~~~"
      reply <- open client "grpcb.in:9000" [] (Timeout 100) encoding decoding
        (singleRequest (Proto3Wire.RPC "grpcbin" "GRPCBin" "Index") EmptyMessage) :: ClientIO (Either TooMuchConcurrency (RawReply IndexReply))

      liftIO $ print reply

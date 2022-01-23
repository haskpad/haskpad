{-|
Module      : Haskpad.Optra.Serialize
Description : Serialization of OperationSeq and Operations.
Copyright   : (c) 2022, Christopher Yoon
License     : GPL-3.0
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskpad.Optra.Serialize
  ( OpInfo (..),
    OpSeqInfo (..),
    fromOpInfo,
    toOpInfo,
    fromOpSeqInfo,
    toOpSeqInfo,
    serializeOp,
    serializeOps,
    deserializeOp,
    deserializeOps,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as DB
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS
import qualified Data.Text.Lazy as DT
import qualified Data.Text.Lazy.Encoding as DTE
import GHC.Generics (Generic)
import Haskpad.Optra.Operation as OP

data OpInfo = OpInfo
  { opType :: DT.Text,
    paramInt :: Int,
    paramStr :: DT.Text
  }
  deriving (Generic, Show)

instance FromJSON OpInfo where
  parseJSON = withObject "OpInfo" $ \v ->
    OpInfo
      <$> v .: "opType"
      <*> v .: "paramInt"
      <*> v .: "paramStr"

instance ToJSON OpInfo where
  toEncoding = genericToEncoding defaultOptions

data OpSeqInfo = OpSeqInfo
  { blen :: Int,
    tlen :: Int,
    ops :: [DT.Text]
  }
  deriving (Generic, Show)

instance FromJSON OpSeqInfo where
  parseJSON = withObject "OpSeqInfo" $ \v ->
    OpSeqInfo
      <$> v .: "blen"
      <*> v .: "tlen"
      <*> v .: "ops"

instance ToJSON OpSeqInfo where
  toEncoding = genericToEncoding defaultOptions

-- | Serialize OP.Operation to JSON
serializeOp :: OP.Operation -> DB.ByteString
serializeOp op = encode $ toOpInfo op

-- | Serialize OP.OperationSeq to JSON
serializeOps :: OP.OperationSeq -> DB.ByteString
serializeOps ops = encode $ toOpSeqInfo ops

-- | Deserialize to OP.Operation from JSON
deserializeOp :: DB.ByteString -> OP.Operation
deserializeOp op = do
  let decoded = decode op :: Maybe OpInfo
  case decoded of
    Just o -> fromOpInfo o
    Nothing -> OP.ErrorOp

-- | Deserialize to OP.OperationSeq from JSON
deserializeOps :: DB.ByteString -> OP.OperationSeq
deserializeOps ops = do
  let decoded = decode ops :: Maybe OpSeqInfo
  case decoded of
    Just o -> fromOpSeqInfo o
    Nothing -> OP.ErrorOpSeq

-- | Convert OpInfo to OP.Operation
fromOpInfo :: OpInfo -> OP.Operation
fromOpInfo (OpInfo opType paramInt paramStr)
  | opType == (DT.pack "retain") = OP.Retain paramInt
  | opType == (DT.pack "delete") = OP.Delete paramInt
  | opType == (DT.pack "insert") = OP.Insert (DT.unpack paramStr)
  | otherwise = OP.NoOp

-- | Convert OP.Operation to OpInfo
toOpInfo :: OP.Operation -> OpInfo
toOpInfo (OP.Retain n) = OpInfo (DT.pack "retain") n (DT.pack "")
toOpInfo (OP.Delete n) = OpInfo (DT.pack "delete") n (DT.pack "")
toOpInfo (OP.Insert s) = OpInfo (DT.pack "insert") 0 (DT.pack s)
toOpInfo OP.NoOp = OpInfo (DT.pack "noop") 0 (DT.pack "")

-- | Convert OpSeqInfo to OP.OperationSeq
fromOpSeqInfo :: OpSeqInfo -> OP.OperationSeq
fromOpSeqInfo (OpSeqInfo blen tlen opsInfo) = OP.OperationSeq blen tlen ops
  where
    ops = DS.fromList $ map (deserializeOp . DTE.encodeUtf8) opsInfo

-- | Convert OP.OperationSeq to OpSeqInfo
toOpSeqInfo :: OP.OperationSeq -> OpSeqInfo
toOpSeqInfo (OP.OperationSeq blen tlen ops) = OpSeqInfo blen tlen encOps
  where
    encOps = map (DTE.decodeUtf8 . serializeOp) (DF.toList ops)

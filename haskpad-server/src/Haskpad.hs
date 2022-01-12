module Haskpad
    ( 
      Operation (..)
    , OperationSeq (..)
    , emptyOpSeq
    , addRetain
    , addDelete
    , addInsert
    , apply
    , inverse
    , transform
    , compose
    , OpInfo (..)
    , OpSeqInfo (..)
    , fromOpInfo
    , toOpInfo
    , fromOpSeqInfo
    , toOpSeqInfo
    , serializeOp
    , serializeOps
    , deserializeOp
    , deserializeOps
    ) where


import Haskpad.Optra.Operation
    (
      Operation (..)
    , OperationSeq (..)
    , emptyOpSeq
    , addRetain
    , addDelete
    , addInsert
    , apply
    , inverse
 
    )
import Haskpad.Optra.Commutative (transform, compose)
import Haskpad.Optra.Serialize 
    (
      OpInfo (..)
    , OpSeqInfo (..)
    , fromOpInfo
    , toOpInfo
    , fromOpSeqInfo
    , toOpSeqInfo
    , serializeOp
    , serializeOps
    , deserializeOp
    , deserializeOps
    )


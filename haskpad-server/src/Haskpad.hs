module Haskpad
  ( Operation (..),
    OperationSeq (..),
    emptyOpSeq,
    addRetain,
    addDelete,
    addInsert,
    apply,
    inverse,
    transform,
    compose,
    OpInfo (..),
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

import Haskpad.Optra.Commutative (compose, transform)
import Haskpad.Optra.Operation
  ( Operation (..),
    OperationSeq (..),
    addDelete,
    addInsert,
    addRetain,
    apply,
    emptyOpSeq,
    inverse,
  )
import Haskpad.Optra.Serialize
  ( OpInfo (..),
    OpSeqInfo (..),
    deserializeOp,
    deserializeOps,
    fromOpInfo,
    fromOpSeqInfo,
    serializeOp,
    serializeOps,
    toOpInfo,
    toOpSeqInfo,
  )

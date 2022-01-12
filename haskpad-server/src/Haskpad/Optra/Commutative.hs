{-
 - Implementation of Commutative operations (compose & transform) for
 - Operational transform.
-}

module Haskpad.Optra.Commutative
    (
      compose
    , transform
    ) where

import qualified Data.Foldable as DF
import qualified Data.List as DL
import qualified Data.Sequence as DS
import Haskpad.Optra.Operation as OP 
    


-- | Merges operations o1 and o2 while preserving changes of both.
--   Satisfies apply(apply(S, o1), o2) - apply(S, compose(o1, o2)).
compose :: OP.OperationSeq -> OP.OperationSeq -> OP.OperationSeq
compose (OP.OperationSeq b1 t1 ops1) (OP.OperationSeq b2 t2 ops2)
    | t1 /= b2  = OP.emptyOpSeq
    | otherwise = composeOps (DF.toList ops1) (DF.toList ops2) OP.emptyOpSeq


-- | Helper function for `compose`.
composeOps 
    :: [OP.Operation] 
    -> [OP.Operation] 
    -> OP.OperationSeq 
    -> OP.OperationSeq
composeOps [] [] opSeq = opSeq

composeOps (o:ops1) [] _ = OP.emptyOpSeq

composeOps [] (o:ops2) _ = OP.emptyOpSeq

composeOps ((OP.Delete n):ops1) ops2 res = composeOps ops1 ops2 res'
  where
    res' = OP.addDelete res n

composeOps ops1 ((OP.Insert s):ops2) res = composeOps ops1 ops2 res'
  where
    res' = OP.addInsert res s

composeOps ((OP.Retain n):ops1) ((OP.Retain m):ops2) res 
    | n < m     = composeOps ops1 ((OP.Retain (m - n)):ops2) resRetn
    | n > m     = composeOps ((OP.Retain (n - m)):ops1) ops2 resRetm
    | otherwise = composeOps ops1 ops2 resRetn
  where
    resRetn = OP.addRetain res n 
    resRetm = OP.addRetain res m

composeOps ((OP.Insert s):ops1) ((OP.Delete n):ops2) res
    | sLen < n  = composeOps ops1 ((OP.Delete (n - sLen)):ops2) res
    | sLen > n  = composeOps ((OP.Insert (drop n s)):ops1) ops2 res 
    | otherwise = composeOps ops1 ops2 res
  where
    sLen = length s

composeOps ((OP.Insert s):ops1) ((OP.Retain n):ops2) res
    | sLen < n  = composeOps ops1 ((OP.Retain (n - sLen)):ops2) resIns
    | sLen > n  = composeOps ops1 ((OP.Retain (sLen - n)):ops2) resIns
    | otherwise = composeOps ops1 ops2 resIns
  where
    sLen   = length s
    resIns = OP.addInsert res s

composeOps ((OP.Retain n):ops1) ((OP.Delete m):ops2) res 
    | n < m     = composeOps ops1 ((OP.Delete (m - n)):ops2) resDeln
    | n > m     = composeOps ((OP.Retain (n - m)):ops1) ops2 resDelm
    | otherwise = composeOps ops1 ops2 resDeln
  where
    resDeln = OP.addDelete res n
    resDelm = OP.addDelete res m


-- | Transforms two operations o1 and o2 that happened concurrently and 
--   produces two operations o1' and o2' such that
--       apply(apply(S, o1), o2') = apply(apply(S, o2), o1').
transform
    :: OP.OperationSeq
    -> OP.OperationSeq
    -> (OP.OperationSeq, OP.OperationSeq)
transform (OP.OperationSeq b1 _ ops1) (OP.OperationSeq b2 _ ops2)
    | b1 /= b2  = (OP.emptyOpSeq, OP.emptyOpSeq)
    | otherwise = transformOps (DF.toList ops1) (DF.toList ops2) res
  where
    res = (OP.emptyOpSeq, OP.emptyOpSeq)


-- | Helper function for `transform`.
transformOps 
    :: [OP.Operation]
    -> [OP.Operation]
    -> (OP.OperationSeq, OP.OperationSeq)
    -> (OP.OperationSeq, OP.OperationSeq)

transformOps [] [] res = res

transformOps (o:ops1) [] _ = (OP.emptyOpSeq, OP.emptyOpSeq)

transformOps [] (o:ops2) _ = (OP.emptyOpSeq, OP.emptyOpSeq)

transformOps ((OP.Insert s):ops1) ops2 res = transformOps ops1 ops2 res'
  where
    (ops1', ops2') = res
    res'           = ((OP.addInsert ops1' s), (OP.addRetain ops2' (length s))) 

transformOps ops1 ((OP.Insert s):ops2) res = transformOps ops1 ops2 res'
  where
    (ops1', ops2') = res
    res'           = ((OP.addRetain ops1' (length s)), (OP.addInsert ops2' s)) 

transformOps ((OP.Retain n):ops1) ((OP.Retain m):ops2) res
    | n < m     = transformOps ops1 (retMN:ops2) (ops1n, ops2n)
    | n > m     = transformOps (retNM:ops1) ops2 (ops1m, ops2m)
    | otherwise = transformOps ops1 ops2 (ops1n, ops2n)
  where
    (ops1', ops2') = res
    (retN, retM)   = ((Retain n), (Retain m))
    (retNM, retMN) = ((Retain (n-m)), (Retain (m-n)))
    (ops1n, ops1m) = ((OP.addRetain ops1' n), (OP.addRetain ops1' m))
    (ops2n, ops2m) = ((OP.addRetain ops2' n), (OP.addRetain ops2' m))

transformOps ((OP.Delete n):ops1) ((OP.Delete m):ops2) res
    | n < m     = transformOps ops1 ((OP.Retain (m - n)):ops2) res
    | n > m     = transformOps ((OP.Retain (n - m)):ops1) ops2 res
    | otherwise = transformOps ops1 ops2 res 

transformOps ((OP.Delete n):ops1) ((OP.Retain m):ops2) res
    | n < m     = transformOps ops1 ((OP.Retain (m - n)):ops2) (ops1n, ops2') 
    | n > m     = transformOps ((OP.Retain (n - m)):ops1) ops2 (ops1m, ops2')
    | otherwise = transformOps ops1 ops2 (ops1n, ops2')
  where
    (ops1', ops2') = res
    (delN, delM)   = (OP.Delete n, OP.Delete m)
    (ops1n, ops1m) = ((OP.addDelete ops1' n), (OP.addDelete ops1' m))

transformOps ((OP.Retain n):ops1) ((OP.Delete m):ops2) res
    | n < m     = transformOps ops1 ((OP.Retain (m - n)):ops2) (ops1', ops2n) 
    | n > m     = transformOps ((OP.Retain (n - m)):ops1) ops2 (ops1', ops2m)
    | otherwise = transformOps ops1 ops2 (ops1', ops2n)
  where
    (ops1', ops2') = res
    (delN, delM)   = (OP.Delete n, OP.Delete m)
    (ops2n, ops2m) = ((OP.addDelete ops2' n), (OP.addDelete ops2' m))


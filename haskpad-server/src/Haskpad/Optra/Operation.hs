module Haskpad.Optra.Operation 
    (
      Operation (..)
    , OperationSeq (..)
    , emptyOpSeq
    , addRetain
    , addDelete
    , addInsert
    , apply
    , inverse
    ) where

import qualified Data.Sequence as DS 
import qualified Data.Foldable as DF
import qualified Data.List as DL


data Operation
    = Retain Int
    | Delete Int
    | Insert String
    | NoOp
    | ErrorOp
    deriving (Eq, Show)


isRetain :: Operation -> Bool
isRetain (Retain _) = True
isRetain _          = False


getRet :: Operation -> Int
getRet (Retain n) = n
getRet _          = 0


isDelete :: Operation -> Bool
isDelete (Delete _) = True
isDelete _          = False


getDel :: Operation -> Int
getDel (Delete n) = n
getDel _          = 0


isInsert :: Operation -> Bool
isInsert (Insert _) = True
isInsert _          = False


getIns :: Operation -> String
getIns(Insert str) = str
getIns _           = ""


isNoOp :: Operation -> Bool
isNoOp NoOp = True
isNoOp _    = False


isErrorOp :: Operation -> Bool
isErrorOp ErrorOp = True
isErrorOp _       = False


data OperationSeq = OperationSeq 
    { baseLen    :: Int
    , targetLen  :: Int
    , operations :: DS.Seq Operation
    } | ErrorOpSeq
    deriving (Eq, Show)


-- | Check ErrorOpSeq
isErrorOpSeq :: OperationSeq -> Bool
isErrorOpSeq ErrorOpSeq = True
isErrorOpSeq _          = False


-- | Return an empty OperationSeq instance.
emptyOpSeq :: OperationSeq
emptyOpSeq = OperationSeq 0 0 DS.Empty 


-- | Add a Retain operation to the OperationSeq. 
addRetain :: OperationSeq -> Int -> OperationSeq
addRetain (OperationSeq blen tlen ops) n
    | n == 0      = OperationSeq blen tlen ops
    | isRetain op = OperationSeq blen' tlen' opsRet 
    | otherwise   = OperationSeq blen' tlen' opsOther
  where
    (fstn, lstn) = ((DS.length ops) - 1, 1)
    (ops', [op])      = (safeTake fstn ops, DF.toList (safeLast lstn ops))
    (blen', tlen')    = (blen + n, tlen + n)
    opsRet            = ops' DS.|> (Retain (n + (getRet op)))
    opsOther          = ops DS.|> (Retain n)


-- | Add a Delete operation to the OperationSeq
addDelete :: OperationSeq -> Int -> OperationSeq
addDelete (OperationSeq blen tlen ops) n
    | n == 0      = OperationSeq blen tlen ops
    | isDelete op = OperationSeq blen' tlen opsDel
    | otherwise   = OperationSeq blen' tlen opsOther
  where
    (fstn, lstn)         = ((DS.length ops) - 1, 1)
    (ops', [op])         = (safeTake fstn ops, DF.toList (safeLast lstn ops))
    blen'                = blen + n
    opsDel               = ops' DS.|> (Delete (n + (getDel op)))
    opsOther             = ops DS.|> (Delete n) 


-- | Add an Insert operation to the OperationSeq
addInsert :: OperationSeq -> String -> OperationSeq
addInsert (OperationSeq blen tlen ops) s
    | DL.null s                    = OperationSeq blen tlen ops
    | isInsert op1 && isDelete op2 = OperationSeq blen tlen' opsInsDel
    | isInsert op2                 = OperationSeq blen tlen' opsIns
    | isDelete op2                 = OperationSeq blen tlen' opsDel
    | otherwise                    = OperationSeq blen tlen' opsOther
  where
    (fstn, lstn)       = ((DS.length ops) - 2, 2)
    (ops', [op1, op2]) = (safeTake fstn ops, DF.toList (safeLast lstn ops))
    opsInsDel          = ops' DS.|> (Insert ((getIns op1) ++ s)) DS.|> op2
    opsIns             = ops' DS.|> op1 DS.|> (Insert ((getIns op2) ++ s))
    opsDel             = ops' DS.|> (Insert s) DS.|> op2
    opsOther           = ops DS.|> (Insert s)
    tlen'              = tlen + (length s)


-- | Apply operations to a String and output resulting string.
--   If OperationSeq is not applicable to string, just return original.
apply :: OperationSeq -> String -> String
apply (OperationSeq blen tlen opSeq) str
    | length str == blen = applyOps (DF.toList opSeq) str ""
    | otherwise          = str
 

-- | Helper function for apply.
applyOps :: [Operation] -> String -> String -> String
applyOps [] str res = res
applyOps ((Retain n):ops) str res = applyOps ops str' res'
  where
    str' = drop n str 
    res' = res ++ (take n str)
    
applyOps ((Delete n):ops) str res = applyOps ops (drop n str) res
     
applyOps ((Insert s):ops) str res = applyOps ops str (res ++ s)

applyOps ((NoOp):ops) str res     = applyOps ops str res

applyOps ((ErrorOp):ops) str res  = str 


-- | Compute inverse of a sequence of operations.
inverse :: OperationSeq -> String -> OperationSeq
inverse (OperationSeq blen tlen opSeq) str
    | elem ErrorOp inv = ErrorOpSeq
    | otherwise           = OperationSeq blen tlen inv
  where
    inv = DS.fromList $ inverseOps (DF.toList opSeq) str []


-- | Helper function for inverse.
inverseOps :: [Operation] -> String -> [Operation] -> [Operation]
inverseOps [] str inv = reverse inv

inverseOps ((Retain n):ops) str inv = inverseOps ops str' inv'
  where
    str' = drop n str
    inv' = ((Retain n):inv)

inverseOps ((Delete n):ops) str inv = inverseOps ops str' inv'
  where
    str' = drop n str
    inv' = ((Insert (take n str)):inv)

inverseOps ((Insert s):ops) str inv = inverseOps ops str inv'
  where
    inv' = ((Delete (length s)):inv)


inverseOps ((NoOp):ops) str inv = inverseOps ops str inv

inverseOps ((ErrorOp):ops) str inv = [ErrorOp]



-- | Safely get last n elements from a Seq; if not enough elements, add NoOp.
safeLast :: Int -> DS.Seq Operation -> DS.Seq Operation
safeLast n s
    | slen < n  = s DS.>< (DS.fromList (take (n - slen) (DL.repeat NoOp)))
    | otherwise = DS.drop (DS.length s - n) s 
  where
    slen = DS.length s

-- | Safely get first n elements from a Seq; if not enough elements, add NoOp.
safeTake :: Int -> DS.Seq Operation -> DS.Seq Operation
safeTake n s
    | slen < n  = s DS.>< (DS.fromList (take (n - slen) (DL.repeat NoOp)))
    | otherwise = DS.take n s
  where
    slen = DS.length s


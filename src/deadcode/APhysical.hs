module APhysical(
	PDelay, PFanout, PSize,
	Physical(..),
	aPhysical
	) where
import List(sort)
import qualified OrdMap as O
import PPrint
import Error(internalError, eshow)
import Id
import Prim
import ASyntax
import ASyntaxUtil(tsortADefs)
import AExpand(aUnExpand)

import Trace

--
-- Compute physical properties
-- pSize p s n 		returns the size of primop p for size s with n arguments
-- pFanin p s n i	returns the fan-in of primop p for size s at input i (out of n inputs)
-- pDelay p s o t	returns the delay of primop p for size s at fanout o, total size t

data Physical = Physical {
	pSize :: PrimOp -> Integer -> Int -> PSize,
	pFanin :: PrimOp -> Integer -> Int -> Int -> PFanout,
	pDelay :: PrimOp -> Integer -> PFanout -> PSize -> PDelay
	}

------------------------

type PDelay = Double

data Node = Var Id | Op PrimOp PDelay | Const
    deriving (Eq, Ord)

instance PPrint Node where
    pPrint d p (Var i) = pPrint d p i
    pPrint d p (Op o w) = pPrint d 0 o -- <> text ":" <> pPrint d 0 w
    pPrint d p (Const) = text "K"

type Path = [Node]
type WPath = (PDelay, Path)
type PathMap = O.OrdMap Id WPath

aPhysical :: Physical -> ASPackage -> IO ()
aPhysical phys p = do
    let (ASPackage _ _ _ os is _ vars vos _ ds fs) = aUnExpand "pp" p
	vs = is ++ vos
	ds' = tsortADefs ds
	tSize = sum (map (szInst phys) vars) + sum (map (szDef phys) ds)
	fan = doEquiv (fanDefs phys emptyFanMapE ds)	-- XXX insts
	zeroPath (i, _) = (i, (1.0, [Var i]))
        paths = O.toList (computePaths phys tSize fan (O.fromList (map zeroPath vs)) ds')
	wpaths = sort (map snd paths)
    putStrLn ("total size " ++ eshow tSize)
    putStr (concat (map ppReadable (O.toList fan)))
    putStr (concat (map ppReadable vs))
    putStr (concat (map ppReadable ds'))
--    putStr (concat (map ppReadable paths))
    putStr (concat (map ppReadable wpaths))
    return ()

------------------------

computePaths :: Physical -> PSize -> FanMap -> PathMap -> [ADef] -> PathMap
computePaths phys sz fan m [] = m
computePaths phys sz fan m (ADef i t e : ds) = 
--	trace ("add " ++ ppReadable i) $
	computePaths phys sz fan (O.add (i, (w, Var i : p)) m) ds
  where (w, p) = getEPath phys sz o m e
	o = case O.lookup i fan of
	    Just f -> f
	    Nothing -> error "computePaths"

getEPath :: Physical -> PSize -> PFanout -> PathMap -> AExpr -> WPath
getEPath phys sz fan m (APrim (ATBit s) p es) = doPrim phys sz fan p s (map (getEPath phys sz fan m) es)
getEPath phys sz fan m (AMethCall _ _ _ es) =
    internalError "APhysical.getEPath AMethCall"
getEPath phys sz fan m (AFunCall _ _ _ es) =
    internalError "APhysical.getEPath AFunCall"
getEPath _ _ _ m (ASVar _ i) = find m i
getEPath _ _ _ m (ASDef _ i) = find m i
getEPath _ _ _ m (ASInt _ _) = (0.0, [Const])
getEPath _ _ _ m (ASStr _ _) = (0.0, [Const])
getEPath _ _ _ m (ASAny _) = (0.0, [Const])

find :: PathMap -> Id -> WPath
find m i =
    case O.lookup i m of
    Just wp -> wp
    Nothing -> internalError ("APhysical.find " ++ ppReadable i ++ ppReadable (O.toList m))

doPrim :: Physical -> PSize -> PFanout -> PrimOp -> Integer -> [WPath] -> WPath
doPrim phys sz fan PrimExtract _ [wp, _, _] = addPrim PrimExtract 0.0 wp
doPrim phys sz fan PrimConcat _ wps = addPrim PrimConcat 0.0 (pickLargest wps)
{-
PrimIf
PrimMux
PrimPriMux
-}
doPrim phys sz o p s wps = addPrim p (pDelay phys p s o sz) (pickLargest wps)

pickLargest :: (Ord a) => [a] -> a
pickLargest = head . reverse . sort

addPrim p w (sw, ps) = (sw+w, Op p w : ps)

------------------------

--
-- Compute the fanout of each definition.
--
type PFanout = Double
type FanMap = O.OrdMap Id PFanout
type FanMapE = (FanMap, [(Id, Id)])

emptyFanMapE :: FanMapE
emptyFanMapE = (O.empty, [])

doEquiv :: FanMapE -> FanMap
doEquiv (fm, iis) =
    let m0 = O.fromList [(i, [i]) | i <- O.indices fm]
	m1 = foldr join m0 iis
	join (i, i') m =
	    let is = find i m ++ find i' m
	    in  O.add (i, is) $ O.add (i', is) $ m
	find i m = case O.lookup i m of Just is -> is; Nothing -> []
	loc i = case O.lookup i fm of Just s -> s; Nothing -> error "doEquiv.loc"
	fm' = O.fromList [ (i, sum (map loc (find i m1))) | i <- O.indices fm]
    in  fm'

addFan :: AId -> PFanout -> FanMapE -> FanMapE
addFan i o (m, iis) = (O.add_C (+) (i, o) m, iis)

addEquiv :: AId -> AId -> FanMapE -> FanMapE
addEquiv i1 i2 (m, iis) = (m, (i1, i2) : iis)

fanInst :: Physical -> FanMapE -> AVInst -> FanMapE
fanInst phys m _ = trace "fanInst" m

fanDefs :: Physical -> FanMapE -> [ADef] -> FanMapE
fanDefs phys m ds = foldr (fanDef phys) m ds

fanDef :: Physical -> ADef -> FanMapE -> FanMapE
fanDef phys (ADef i _ e) m = fanExpr phys m i e

fanExpr :: Physical -> FanMapE -> AId -> AExpr -> FanMapE
--fanExpr phys m _ (APrim (ATBit s) p es) = wirePrim!!
fanExpr phys m _ (APrim (ATBit s) p es) =
    let n = length es
	f = pFanin phys p s n
    in  foldr fanSExpr m (zip es [ f i | i <- [0..n-1] ])
fanExpr phys m _ (AMethCall _ _ _ es) = m		-- handled by AInst code
fanExpr phys m _ (AFunCall _ _ _ es) = trace "fanExpr AFunCall" m
fanExpr phys m i (ASVar _ i') = addEquiv i i' m
fanExpr phys m i (ASDef _ i') = addEquiv i i' m
fanExpr _ m _ _ = m

fanSExpr :: (AExpr, PFanout) -> FanMapE -> FanMapE
fanSExpr (ASVar _ i, o) m = addFan i o m
fanSExpr (ASDef _ i, o) m = addFan i o m
fanSExpr _ m = m

------------------------

type PSize = Double

szInst :: Physical -> AVInst -> PSize
szInst phys (AVInst {}) = trace "szInst" 0.0	-- XXX size of the instance

szDef :: Physical -> ADef -> PSize
szDef phys (ADef _ _ e) = szExpr phys e

szExpr :: Physical -> AExpr -> PSize
szExpr phys (APrim (ATBit s) p es) = 
	if wirePrim p then 0.0 else pSize phys p s (length es)
szExpr phys (AMethCall {}) = 0.0		-- Only wires
szExpr phys (AFunCall {}) = trace "szExpr AFunCall" 0.0
szExpr phys (ASInt {}) = 0.0			-- Only wires
szExpr phys (ASVar {}) = 0.0			-- Only wires
szExpr phys (ASDef {}) = 0.0			-- Only wires
szExpr phys (ASStr {}) = 0.0			-- Only wires
szExpr phys (ASAny {}) = 0.0			-- Only wires

------------------------

wirePrim PrimExtract = True
wirePrim PrimConcat = True
wirePrim PrimZeroExt = True
wirePrim PrimSignExt = True	-- XXX fanin
wirePrim _ = False

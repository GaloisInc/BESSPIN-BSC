module ADisjoint(id_ADisjoint, ExprDisjointTest, mkExprDisjointTest) where

import Id(Id, getIdString)
import ASyntax
import Prim(PrimOp(..))
import Error(internalError)
import qualified BDD as B
import qualified OrdMap as M
import PPrint
import IntLit
import Flags
import Util(traces, mergeWith, commonElts, cmpFst, uniquePairs, ordPair)
import ListUtil(mapSnd)
import SMonad
import Intervals
import PreIds(tmpVarSIds)
import IntegerUtil
import AExpr2Bdd(RuleDisjointTest)

import IOUtil(progArgs)

id_ADisjoint = "$Id: ADisjoint.hs,v 1.34 2003/01/13 20:45:03 elf Exp $"

type ExprDisjointTest = ASExpr -> ASExpr -> Bool

-- assumptions: no duplicate definitions

trace_disjoint_tests = "-trace-disjoint-tests" `elem` progArgs

type BDD = B.BDD Id

type ValueSet = [(Id, (VSetInteger, Integer))]		-- possible value, upper bound

type Info = (BDD, ValueSet)

data State = State [Id] (M.OrdMap Id ADef) (M.OrdMap Id Info)
type S a = SM State a

freshVar :: S Id
freshVar = M $ \(State (i:is) defs m) -> (State is defs m, i)

bindVar :: Id -> Info -> S Info
bindVar id info = M $ \(State ids defs m) ->
		     case M.lookup id m of
		     Nothing -> (State ids defs (M.add (id,info) m), info)
		     Just _  -> internalError $ "ADisjoint.bindVar: Identifier "
				++ ppReadable id ++ " defined more than once"

getBDDMap :: S (M.OrdMap Id Info)
getBDDMap = M $ \ s@(State ids defs m) -> (s, m)

getDefMap :: S (M.OrdMap Id ADef)
getDefMap = M $ \ s@(State ids defs m) -> (s, defs)

deleteDef :: Id -> S ()
deleteDef d = M $ \ s@(State ids defs m) -> (State ids (M.delete d defs) m, ())

----

convSExpr :: ASExpr -> S Info
convSExpr (ASInt (ATBit 1) (IntLit { ilValue = 0 })) = return (B.bddFalse, [])
convSExpr (ASInt (ATBit 1) (IntLit { ilValue = 1 })) = return (B.bddTrue, [])
convSExpr (ASVar _ id) = return $ (B.bddVar id, []) -- external variables
convSExpr e@(ASDef (ATBit 1) id) = do
	m <- getBDDMap
	case M.lookup id m of
	 Just inf -> return inf
	 Nothing  -> convDef id
convSExpr sexpr@(ASAny (ATBit 1)) = return (B.bddFalse, [])	-- XXX why is this here anyway?
convSExpr sexpr@(ASAny _) = internalError $ "ADisjoint.convSExpr (any): " ++ ppReadable sexpr
convSExpr sexpr@(ASInt _ _) = internalError $ "ADisjoint.convSExpr (int): " ++ ppReadable sexpr
convSExpr sexpr@(ASStr _ _) = internalError $ "ADisjoint.convSExpr (str): " ++ ppReadable sexpr
convSExpr sexpr@(ASDef _ _) = internalError $ "ADisjoint.convSExpr (def): " ++ ppReadable sexpr

convExpr :: AExpr -> S Info
convExpr (ASimp sexp) = convSExpr sexp
convExpr (APrim (ATBit 1) op [ASDef _ id, ASInt (ATBit sz) (IntLit { ilValue = val })]) | op `elem` rangeOps =
    do fv <- freshVar
       return (B.bddVar fv, opRangeIdVal op (2^sz) id val)
convExpr (APrim (ATBit 1) op [ASInt (ATBit sz) (IntLit { ilValue = val }), ASDef _ id]) | op `elem` rangeOps =
    do fv <- freshVar
       return (B.bddVar fv, opRangeValId op (2^sz) val id)
convExpr (APrim (ATBit 1) op xprs)
    | op `elem` boolOps = mapM convSExpr xprs >>= (return . convOp op)
    | otherwise =
	do fv <- freshVar
	   return (B.bddVar fv, [])
convExpr _ = do fv <- freshVar
		return (B.bddVar fv, [])

convDefs :: S ()
convDefs = do
    defs <- getDefMap
    case M.indices defs of
     [] -> return ()
     d:_ -> convDef d >> convDefs

convDef :: Id -> S Info
convDef d = do
    defs <- getDefMap
    deleteDef d
    case M.lookup d defs of
     Just (ADef _ (ATBit 1) expr) -> do info <- convExpr expr; bindVar d info
     Just _ -> do v <- freshVar
	          bindVar d (B.bddVar v, [])
     _ -> internalError ("ADisjoint.convDef: no definition or circular definition for " ++ ppReadable d)

mkExprDisjointTest :: Flags -> [ADef] -> ExprDisjointTest
mkExprDisjointTest flags defs = 
  let initstate = State tmpVarSIds (M.fromList [(id,d) | d@(ADef id _ _) <- defs]) M.empty
      (s, vss) = run initstate convDefs
      disjointExprs e1 e2 =
                 let (s', (bdd1, vs1)) = run s  (convSExpr e1) 
                     (_,  (bdd2, vs2)) = run s' (convSExpr e2)
                     disjointBDD = B.bddIsFalse (B.bddAnd bdd1 bdd2)
                     disjointRanges = any vNull [vs | (_, (vs, _)) <- intervalAnd vs1 vs2]
                     tracer = traces ("disjoint testE2: " ++ (ppReadable e1) ++ " " ++ (ppReadable e2))
                 in
                     (if trace_disjoint_tests then tracer else id)
                     (disjointRanges || (optSchedBDD flags && disjointBDD))
  in
      disjointExprs
    

boolOps :: [PrimOp]
boolOps = [PrimBNot, PrimBAnd, PrimBOr, PrimIf]

convOp :: PrimOp -> [Info] -> Info
convOp PrimBNot [(b,vs)] = (B.bddNot b, mapSnd vsInv vs)
convOp PrimBAnd xs = foldl1 (\(b1,vs1) (b2,vs2) -> (B.bddAnd b1 b2, intervalAnd vs1 vs2)) xs
convOp PrimBOr xs =
    foldl1 (\(b1,vs1) (b2,vs2) -> (B.bddOr b1 b2, intervalOr vs1 vs2)) xs
convOp PrimIf [(c, _), (t,vt), (e,ve)] =
    ((c `B.bddAnd` t) `B.bddOr` (B.bddNot c `B.bddAnd` e), intervalOr vt ve)

vsInv (s, ub) = (vCompRange 0 (ub-1) s, ub)

intervalAnd :: ValueSet -> ValueSet -> ValueSet
intervalAnd = mergeWith (\ (s, ub) (s', ub') -> if ub == ub' then (vIntersect s s', ub) else internalError "intervalAnd")

intervalOr :: ValueSet -> ValueSet -> ValueSet
intervalOr vs1 vs2 =
    [ if ub == ub' then (id, (vUnion v v', ub)) else internalError "intervalOr" |
      ((id, (v, ub)), (_, (v', ub'))) <- commonElts cmpFst vs1 vs2]

rangeOps :: [PrimOp]
rangeOps = [PrimEQ, PrimULE, PrimULT] --, PrimSLE, PrimSLT]

opRangeIdVal :: PrimOp -> Integer -> AId -> Integer -> ValueSet
opRangeIdVal PrimEQ  ub id val = [(id, (vSing val, ub))]
opRangeIdVal PrimULE ub id val = [(id, (vFromTo 0 val, ub))]
opRangeIdVal PrimULT ub id val = [(id, (vFromTo 0 (val-1), ub))]
--opRangeIdVal PrimSLE ub id val = [(id, vFromTo 0 val     `vUnion` ??? )]
--opRangeIdVal PrimSLT ub id val = [(id, vFromTo 0 (val-1) `vUnion` ???)]

opRangeValId :: PrimOp -> Integer -> Integer -> AId -> ValueSet
opRangeValId PrimEQ  ub val id = [(id, (vSing val, ub))]
opRangeValId PrimULE ub val id = [(id, (vFromTo val (ub-1), ub))]
opRangeValId PrimULT ub val id = [(id, (vFromTo (val+1) (ub-1), ub))]
--opRangeValId PrimSLE ub val id = [(id, vFrom val) ???]
--opRangeValId PrimSLT ub val id = [(id, vFrom (val+1)) ???]

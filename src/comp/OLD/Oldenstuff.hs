module Oldenstuff where
import ISyntax
import Prim
--import ErrorUtil(internalError)
import PPrint(ppReadable, ppString)
import PreIds(idPrimNoExpIf)
import Flags(Flags, expandIf)
import ISyntaxUtil
import Id
import Pragma(SPIdSplitMap, splitSchedPragmaIds)
import ITransform(optIRule)
import PreStrings(fs_T, fs_F)
--import Trace(trace)
import Error

icPrimNoExpIf = ICon idPrimNoExpIf (ICPrim { iConType = itAction `itFun` itAction, primOp = PrimNoExpIf })

-- flattens actions and pushes nosplits (i.e. deep nosplitting tokens)
-- even inside ifs - because otherwise they are handled too late
flatActionNosplit :: IExpr a -> [IExpr a]
flatActionNosplit (IAps (ICon _ (ICPrim { primOp = PrimJoinActions })) _ [a1, a2]) = flatActionNosplit a1 ++ flatActionNosplit a2
flatActionNosplit (ICon _ (ICPrim { primOp = PrimNoActions })) = []
flatActionNosplit (IAps (ICon _ (ICPrim { primOp = PrimNosplitDeep })) _ [a]) = flatActionNosplit (pushNosplit a)
flatActionNosplit (IAps (ICon _ (ICPrim { primOp = p })) _ [e]) | (p == PrimExpIf || p == PrimNoExpIf) && notIf e = flatActionNosplit e
flatActionNosplit (ICon i (ICUndet { })) = {- trace "_ action" $ -} []
flatActionNosplit (IAps (ICon i (ICUndet { })) _ _) = {- trace "_ action" $ -} []
flatActionNosplit (IAps iF@(ICon _ (ICPrim { primOp = PrimIf })) [typ] [c, t, f]) | typ == itAction =
   [(IAps iF [typ] [c, (joinActions (flatActionNosplit t)), (joinActions (flatActionNosplit f))])]
flatActionNosplit a = [a]

pushNosplit :: IExpr a -> IExpr a
pushNosplit (IAps wrapper@(ICon _ (ICPrim { primOp = PrimNoExpIf })) t [e])
      -- the type argument t should be [] but we are not checking it for now
      = (IAps wrapper t [(pushNosplit e)])
pushNosplit (IAps (ICon _ (ICPrim { primOp = PrimNosplitDeep })) _t [e])
      -- the type argument _t should be [] but we are not checking it for now
      = (IAps icPrimNoExpIf  [] [(pushNosplit e)])
pushNosplit (IAps wrapper@(ICon _ (ICPrim { primOp = PrimExpIf })) t1
             [IAps iF@(ICon _ (ICPrim { primOp = PrimIf })) t2 es2])
      = (IAps wrapper t1
-- not checking if t2 matches [_], i.e. a singleton list
-- indeed t2 should be [itAction]
-- perhaps it should, but that is a task for another day
-- t1 should be []
              [IAps iF t2 (map pushNosplit es2)])
pushNosplit (IAps (ICon _ (ICPrim { primOp = PrimExpIf })) _ [e])
-- primExpIf wrapped around a "non-If".  discard the wrapper (ought to emit a warning?)
      = pushNosplit e
pushNosplit (IAps (ICon _ (ICPrim { primOp = op })) _ _)
      | (op == PrimNoExpIf || op == PrimNosplitDeep || op == PrimExpIf)
      = internalError ( (ppReadable op) ++ " called with wrong number of arguments")
pushNosplit (IAps iF@(ICon _ (ICPrim { primOp = PrimIf })) [t] es) | t == itAction =
	IAps icPrimNoExpIf [] [IAps iF [t] (map pushNosplit es)]
-- not pushNosplit into f because...?
pushNosplit (IAps f ts es) = IAps f ts (map pushNosplit es)
pushNosplit e = e

iExpandIfRules :: Flags -> IRules a -> IRules a
iExpandIfRules flags (IRules sps rs) = IRules sps' rs'
  where xs = unzip (map (iExpandIfRule flags) rs)
	idsplitmap = concat (fst xs)
	rs' = concat (snd xs)
	sps' = splitSchedPragmaIds idsplitmap sps

-- flatActionNosplit needed to process explicit nosplit tokens
-- for ones that might be buried inside ifs
iExpandIfRule :: Flags -> IRule a -> (SPIdSplitMap, [IRule a])
iExpandIfRule flags r@(IRule i rps s wp p a orig) =
    -- trace ("iExpandIfRule " ++ ppReadable r) $
    -- trace ("iExpandIfRule2 " ++ ppReadable (flatActionNosplit a)) $
    case exp1 [] (flatActionNosplit a) of
        Left as -> ([], [IRule i rps s wp p (joinActions as) orig])
        Right (c, at, af) -> 
	    --trace (ppReadable (r, (c, at, af))) $
	    let
		-- recursively call iExpandIfRule
		expOpt r = iExpandIfRule flags (optIRule flags r)

		splitorig = maybe (Just i) Just orig

		t_id = (mkSplitId i fs_T)  -- id of the True rule
		f_id = (mkSplitId i fs_F)  -- id of the False rule
		t_p  = (p `ieAnd` c)       -- pred of the True rule
		f_p  = (p `ieAnd` ieNot c) -- pred of the False rule
	        t_rule = IRule t_id rps (s ++ "_T") wp t_p at splitorig
	        f_rule = IRule f_id rps (s ++ "_F") wp f_p af splitorig

                -- recursive expansion calls
		(idmap1, rs1) = expOpt t_rule
		(idmap2, rs2) = expOpt f_rule

                idmap = mkIdMap i t_id f_id idmap1 idmap2
	    in
		(idmap, rs1 ++ rs2)
  where
    isExpandIf = expandIf flags

    -- Given an empty list of actions a list of actions to consider,
    -- return either actions not to be split or a triple:
    --   * the condition on which to split
    --   * the actions in the true branch
    --   * the actions in the false branch
    exp1 :: [IExpr a] -> [IExpr a] -> Either [IExpr a] (IExpr a, IExpr a, IExpr a)
    exp1 as [] = Left as
    exp1 as (a@(IAps (ICon _ (ICPrim { primOp = PrimIf })) _ [c, t, f]) :as')
	= if isExpandIf then mkTriple as c t f as' else dontExp as a as'
    exp1 as (a@(IAps (ICon _ (ICPrim { primOp = PrimExpIf })) _
		[IAps (ICon _ (ICPrim { primOp = PrimIf })) _ [c, t, f]]) :as')
	= mkTriple as c t f as'
    exp1 as (a@(IAps (ICon _ (ICPrim { primOp = PrimNoExpIf })) _
		[s@(IAps (ICon _ (ICPrim { primOp = PrimIf })) _ [c, t, f])]) :as')
	= dontExp as s as'
    exp1 as (s:as') = exp1 (as++[s]) as'

    mkTriple as c t f as' = Right (c, joinActions (as ++ [t] ++ as'),
				      joinActions (as ++ [f] ++ as'))

    dontExp as (IAps ff ts [c, t, f]) as' = 
	exp1 (as++[IAps ff ts [c, chk t, chk f]]) as' 
    dontExp _ _ _ = internalError "dontExp bad case"
 
    chk e@(IAps (ICon ff prim@(ICPrim { primOp = PrimIf })) [typ] [c, t, f])
	| isExpandIf && (isActionType typ)
	= errExp ff e
    chk e@(IAps (ICon _ (ICPrim { primOp = PrimExpIf })) _
	[IAps (ICon ff (ICPrim { primOp = PrimIf })) _ [c, t, f]])
	= errExp ff e
    chk (IAps (ICon _ (ICPrim { primOp = PrimNoExpIf })) _
        [s@(IAps ff@(ICon _ (ICPrim { primOp = PrimIf })) ts [c, t, f])])
	= IAps ff ts [c, chk t, chk f]
    -- noexps not around an if should be stripped
    chk (IAps (ICon _ (ICPrim { primOp = PrimNoExpIf })) _ [e]) = chk e
    chk (IAps f ts es) = IAps f ts (map chk es)
    chk e = e

    errExp f e = unsafeMessageExit serror
                     [(getIdPosition f, ESplitInsideNoSplit (ppString e))]

    -- Not to be confused with makeIdMap!!
    -- This makes a longname map for splitting scheduling pragmas.
    mkIdMap i t_id f_id idmap_t idmap_f =
	let
            -- the names of the rules which t_rule expanded into
            true_rIds =
		case idmap_t of
	            [] -> [t_id]
		    [(_, names)] -> names
		    _ -> internalError
			     ("AConv.iExpandIfRule: " ++
			      "idmap_t contains more than one element")

            -- the names of the rules which f_rule expanded into
            false_rIds =
		case idmap_f of
	            [] -> [f_id]
		    [(_, names)] -> names
		    _ -> internalError
			     ("AConv.iExpandIfRule: " ++
			      "idmap_f contains more than one element")
	in
	   [(i, true_rIds ++ false_rIds)]

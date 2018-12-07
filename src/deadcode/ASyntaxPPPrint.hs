{-# LANGUAGE TypeSynonymInstances #-}
module ASyntaxPPPrint(ppAPackage) where

import Util(itos)
import ErrorUtil(internalError)
import PPrint
import Id
import IdPrint
import PreIds(idInout_)
import ASyntax
import Pragma
import Prim
import VModInfo
import IntLit

-- Even prettier printing
class (PPrint a) => PPPrint a where
    ppPrint :: Int -> a -> Doc
    ppPrint p x = pPrint PDReadable p x

ppAPackage :: APackage -> Doc
ppAPackage ap = ppPrint 0 ap

instance PPPrint Id
instance PPPrint Integer
instance PPPrint Double
instance PPPrint Bool
instance PPPrint RulePragma
instance PPPrint PrimOp
instance PPPrint VName

instance (PPPrint a, PPPrint b) => PPPrint (a, b) where
    ppPrint _ (x, y) = text "(" <> sep [ppPrint 0 x <> text ",", ppPrint 0 y] <> text ")"

instance (PPPrint a, PPPrint b, PPPrint c) => PPPrint (a, b, c) where
    ppPrint _ (x, y, z) = text "(" <> sep [ppPrint 0 x <> text ",", ppPrint 0 y <> text ",", ppPrint 0 z] <> text ")"

instance (PPPrint a) => PPPrint [a] where
    ppPrint _ [] = text "[]"
    ppPrint _ xs = let	(y:ys) = reverse (map (ppPrint 0) xs)
			ys' = map (<> text ",") ys
			xs' = reverse (y:ys')
		   in   text "[" <> sep xs' <> text "]"

instance PPPrint APackage where
    ppPrint _ apkg = --
--	(ppId PDReadable mi <+> text "::" <+> text "???") $+$
	(ppId PDReadable (apkg_name apkg) <+>
         sep (map (ppId PDReadable . fst)
	          (concatMap absInputToPorts (apkg_inputs apkg))) <+>
	 text "= module") $+$
	text "" $+$
	text "-- AP state elements" $+$
	foldr ($+$) (text "") (map (ppPrint 0) (apkg_state_instances apkg)) $+$
	text "-- AP local definitions" $+$
	foldr ($+$) (text "") (map (ppPrint 0) (apkg_local_defs apkg)) $+$
	text "-- AP rules" $+$
	(if null (apkg_rules apkg) then text "" else text "rules" $+$
         (text "" <+> foldr1 ($+$) (map (ppPrint 0) (apkg_rules apkg)))) $+$
	text "" $+$
	text "-- AP scheduling pragmas" $+$
	ppPrint 0 (apkg_schedule_pragmas apkg) $+$
	text "" $+$
	text "interface" $+$
	(text "" <+>
         foldr ($+$) (text "")(map (ppPrint 0) (apkg_interface apkg)))

instance PPPrint AVInst where
    ppPrint _ (AVInst i t ui mts pts vi es ns) =
	let --(ps, es') = splitAt (fromInteger (vNParam vi)) es
            es' = es
	    -- f s = text "·" <> s
        in  ppPrint 0 i <+> text "::" <+> ppPrint 0 t <+> text "<-" <+>
		sep (ppPrint 20 (vName vi) :
                {-map (f . ppPrint 20) ps ++ -}map (ppPrint 20) es') <> text ";"


{-
-- if enabled need to add PPPrinting for AForeignCall to ASPackage and a sep instance

instance PPPrint ASPackage where
    ppPrint p (ASPackage mi fmod ps exps is sch ss sos ds) =
	(text "package" <+> ppId PDReadable mi <> if fmod then text " -- function" else text "") $+$
	text "-- package parameters" $+$
	(text "params" <+> sep (map (ppPrint 0) ps) <> text ";") $+$
	text "-- package outputs" $+$
	(text "export" <+> sep (map (ppPrint 0) exps) <> text ";") $+$
	text "-- package inputs" $+$
	foldr ($+$) (text "") (map pppV is) $+$
	text "-- schedule" $+$
	ppPrint 0 sch $+$ text "" $+$
	text "-- state elements" $+$
	foldr ($+$) (text "") (map (ppPrint 0) ss) $+$
	text "-- state elements outputs" $+$
	foldr ($+$) (text "") (map pppV sos) $+$
	text "-- definitions" $+$
	foldr ($+$) (text "") (map (ppPrint 0) ds)

instance PPPrint ASchedule where
     ppPrint p (ASchedule groups order) = (text "parallel:" <+> ppPrint 0 groups)
					   $+$ (text "order:" <+> ppPrint 0 order)

instance PPPrint AScheduler where
     ppPrint p (ASchedEsposito fs) = text "esposito:" <+>
				       sep [ppPrint 0 r <+> text "->" <+> ppPrint 0 cfs | (r,cfs) <- fs]
-}

instance PPPrint ADef where
    ppPrint _ (ADef i t e) =
	(ppPrint 0 i <> sep [text " ::" <+> ppPrint 0 t,
	                           text "=" <+> ppPrint 0 e <> text ";"])

instance PPPrint AIFace where
    -- XXX print assumptions
    ppPrint p (AIDef mid is wp g (ADef _ t e) _ _) =
--	(text "--" <+> ppPrint p g) $+$
	ppPrint 0 mid <+> sep (map pppV is) <+> text "::" <+> ppPrint 0 t <+> text "=" <+> ppPrint 0 e <> text ";"
    ppPrint p (AIAction is wp g _ rs _) =
	let s = (reverse . drop 4 . reverse) $ ppString g in
--	(text "--" <+> ppPrint p g) $+$
	(text s <+> sep (map pppV is) <+> text ":: PrimAction = ") $+$
--	(text " rules") $+$
	if null rs then text "noAction" else (text "  " <> foldr1 ($+$) (map (ppPrint 0) rs))
    ppPrint p (AIActionValue is wp g _ rs (ADef i t e) _) =
	let s = (reverse . drop 4 . reverse) $ ppString g in
        ppPrint 0 i <+> sep (map pppV is) <+> text "::" <+> ppPrint 0 t <+> text "=" <+> ppPrint 0 e <>
--	(text "--" <+> ppPrint p g) $+$
	(text s <+> sep (map pppV is) <+> text ":: PrimAction = ") $+$
--	(text " rules") $+$
	if null rs then text "noAction" else (text "  " <> foldr1 ($+$) (map (ppPrint 0) rs))
    ppPrint p (AIClock i c _) = pPrint PDReadable p c
    ppPrint p (AIReset i r _) = pPrint PDReadable p r
    ppPrint p (AIInout i r _) = pPrint PDReadable p r

pppV (i, t) = pparen True (ppId PDReadable i <+> text "::" <+> ppPrint 0 t)

instance PPPrint ARule where
    -- XXX print assumptions
    ppPrint _ (ARule s rps sd wp p as _ _) =
	vcat (map (ppPrint 0) rps) $+$
	(ppPrint 0 s <> text ":") $+$
	(text " when" <+> ppPrint 0 p) $+$
	(text "  ==>" <+> pppActions as <> text ";")

pppActions as = text "{" <+> sep (map ppA as) <+> text "}"
	where ppA a = ppPrint 0 a <> text ";"

instance PPPrint AAction where
    ppPrint _ (ACall i m (c : es)) | isOne c = ppPrint 0 i <> text "." <> ppMethId PDReadable m <+> sep (map (ppPrint 1) es)
    ppPrint _ (ACall i m (c : es)) = sep [
	text "if" <+> ppPrint 0 c <+> text "then",
	nest 2 (ppPrint 0 i <> text "." <> ppMethId PDReadable m <+> sep (map (ppPrint 1) es))
	]
    ppPrint _ (AFCall i _ _ (c : es) _) | isOne c = ppPrint 0 i <+> sep (map (ppPrint 1) es)
    ppPrint _ (AFCall i _ _ (c : es) _) = sep [
	text "if" <+> ppPrint 0 c <+> text "then",
	nest 2 (ppPrint 0 i <+> sep (map (ppPrint 1) es))
	]
    ppPrint _ (ATaskAction i _ _ n (c : es) _ _ _) | isOne c = ppPrint 0 i <> (text ("#" ++ itos(n))) <+> sep (map (ppPrint 1) es)
    ppPrint _ (ATaskAction i _ _ n (c : es) _ _ _) = sep [
	text "if" <+> ppPrint 0 c <+> text "then",
	nest 2 (ppPrint 0 i <> (text ("#" ++ itos(n))) <+> sep (map (ppPrint 1) es))
	]
    ppPrint  _ x = internalError ("ppPrint AAction: " ++ show x)

instance PPPrint AExpr where
    ppPrint p (APrim _ _ PrimConcat es) =
         pparen(p>0) $ braces $ hsep ( punctuate comma docArgs )
             where
               docArgs = map (ppPrint 1) es
    ppPrint p (APrim _ _ o es@(_:_:_)) | binOp o =
      pparen (p>0) $ sepList (map (ppPrint 1) es) (text "" <+> ppPrint 1 o)
    ppPrint p (APrim _ _ PrimCase (e:dd:ces)) =
	(text "case" <+> ppPrint 0 e <+> text "of") $+$
	foldr ($+$) (text "_ ->" <+> ppPrint 0 dd) (f ces)
	  where f [] = []
		f (x:y:xs) = (ppPrint 0 x <+> text "->" <+> ppPrint 0 y) : f xs
    		f x = internalError ("ppPrint AExpr binOp: " ++ show x)

    ppPrint p (APrim _ _ PrimPriMux es) = pparen (p>0) $
	text "primux" <+> sep (f es)
	  where f [] = []
		f (x:y:xs) = pparen True (sep [ppPrint 0 x <> text ",", ppPrint 0 y]) : f xs
		f x = internalError ("PPPrint AExpr: " ++ show x)

    ppPrint p (APrim _ _ PrimMux es) = pparen (p>0) $
	text "mux" <+> sep (f es)
	  where f [] = []
		f (x:y:xs) = pparen True (sep [ppPrint 0 x <> text ",", ppPrint 0 y]) : f xs
		f x = internalError ("PPPrint APrim PriMux: " ++ show x)
    ppPrint p (APrim _ _ PrimExtract (var:hi:lo:[])) =
         pparen(p>0) $ ppPrint 1 var <+> lbrack
                       <> (if ( dhi == dlo )
                           then dhi <> rbrack
                           else dhi <> colon <> dlo <> rbrack )
                       where
                        dhi = ppPrint 0 hi
                        dlo = ppPrint 0 lo
    ppPrint p (APrim _ _ PrimIf (cond:thn:els:[])) =
         pparen(p>0) $ ppPrint 1 cond <+> text "?"
                       <+> ppPrint 1 thn <+> colon
                       <+> ppPrint 1 els
    ppPrint p (APrim _ _ o es) = pparen (p>0) $ ppPrint 1 o <+> sep (map (ppPrint 1) es)
    ppPrint p (ANoInlineFunCall _ i _ es)  = pparen (p>0) $ ppPrint 1 i <+> sep (map (ppPrint 1) es)
    ppPrint p (AFunCall _ i _ _ es)  = pparen (p>0) $ ppPrint 1 i <+> sep (map (ppPrint 1) es)
    ppPrint p (ATaskValue _ i _ _ n) = pparen (p>0) $ ppPrint 1 i <> (text ("#" ++ itos(n)))
    ppPrint p (AMethCall _ i m es) =
        pparen (p>0 && not (null es)) $
               ppPrint 1 i <> sep (text "." <> ppMethId PDReadable m : map (ppPrint 1) es)
    ppPrint p (AMethValue _ i m) =
        pparen (p>0) $ ppPrint 1 i <> text "." <> ppMethId PDReadable m
    ppPrint p (ASPort _ i) = ppPrint p i
    ppPrint p (ASParam _ i) = ppPrint p i
    ppPrint p (ASDef _ i) = ppPrint p i
    ppPrint p (ASInt _ (ATBit sz) i) = text (showSizedVeriIntLit sz i)
    ppPrint p (ASInt _ _ i) = text (showVeriIntLit i)
    ppPrint p (ASReal _ _ r) = ppPrint p r
    ppPrint p (ASStr _ _ s) = text (show s)
    ppPrint p (ASAny t _) = pppExprType t $ text "_"
    -- XXX definition expansion ever required for clocks and resets?
    ppPrint p r@(ASReset { }) = pPrint PDReadable p r
    ppPrint p r@(ASInout { }) = pPrint PDReadable p r
    ppPrint p c@(ASClock { }) = pPrint PDReadable p c
    ppPrint p g@(AMGate { }) = pPrint PDReadable p g

pppExprType t e = text "(" <> e <+> text "::" <+> ppPrint 0 t <> text ")"

instance PPPrint AType where
    ppPrint p (ATBit n) = text ("Bit " ++ itos n)
    ppPrint p (ATReal) = text ("Real ")
    ppPrint p (ATString Nothing) = text "String (unsized)"
    ppPrint p (ATString (Just n)) = text ("String (" ++ (itos n) ++ " chars)")
    ppPrint p (ATAbstract i ns) = sep (ppPrint 0 i : map (ppPrint 0) ns)

instance PPPrint ASchedulePragma where
    ppPrint p (SPUrgency ids) =
	text ("{-# ASSERT descending urgency: " ++ show ids ++ "#-}")
    ppPrint p (SPExecutionOrder ids) =
	text ("{-# ASSERT execution order: " ++ show ids ++ "#-}")
    ppPrint p (SPMutuallyExclusive ids) =
	text ("{-# ASSERT mutually exclusive: " ++ show ids ++ "#-}")
    ppPrint p (SPConflictFree ids) =
	text ("{-# ASSERT conflict-free: " ++ show ids ++ "#-}")
    ppPrint p (SPPreempt ids1 ids2) =
	text ("{-# ASSERT preempt: " ++ show ids1 ++ " " ++ show ids2 ++ "#-}")
    ppPrint p (SPSchedule s) =
	text ("{-# ASSERT schedule: " ++ show s ++  "#-}")

-- --------------------

-- XXX duplicated from ASyntax

ppMethId d@PDReadable m = ppId d (unQualId m)
ppMethId d m = ppId d m

isOne (ASInt _ _ (IntLit _ _ 1)) = True
isOne _                          = False

absInputToPorts :: AAbstractInput -> [AInput]
absInputToPorts (AAI_Port p) = [p]
absInputToPorts (AAI_Clock osc Nothing) = [(osc, aTBool)]
absInputToPorts (AAI_Clock osc (Just gate)) = [(osc, aTBool), (gate, aTBool)]
absInputToPorts (AAI_Reset r) = [(r,aTBool)]
absInputToPorts (AAI_Inout r n) = [(r,ATAbstract {ata_id = idInout_, ata_sizes = [n]})]


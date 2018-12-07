module AExpandP(aExpandP) where

import qualified Data.Map as M
import ASyntax
import ASyntaxUtil
import AExpand(aRemoveUnused)

-- import Trace


-- ==============================

-- aExpandP is used prior to pretty printing an unexpanded APackage.

-- APackage early in the compiler has defs (ds) which are simple, like:
--     _d1 = a + _d2
--     _d2 = b + _d3
--     _d3 = c + d
-- while it would be easier for the user to read if these were expanded
-- into a complex expression:
--     _d1 = a + b + c + d
--
-- aExpandP currently expands *all* definitions.
-- Look at revision 3176 of CCode.hs to see how this code used to decide
-- what to expand -- by having expDef selectively decide to add the def
-- to the sm map or by considering whether the expression should always
-- be expanded (such as some PrimExtract/PrimBNot, expression sizes, etc)
-- and by having expE do special things for PrimBAnd/Or.
--

-- History:
--
-- When the -ATS flag was added by Lennart, there existed APackage
-- (which only had simple ds) and AXPackage (which allowed complex ds).
-- In the C backend, the pass "aExpandC" expanded out an APackage to
-- AXPackage.  So when -ATS was added, presumably Lennart re-used
-- "aExpandC" to make the ATS more readable before ppPrint, rather
-- than writing a separate function.
--
-- Since then, aExpandC has taken on new functionality as part of the
-- C backend, and so at some point aExpandP was separated off for use
-- just with the -ATS flag.
-- 
-- So some code here might have been relevant only to the C backend
-- but was inadvertently not removed when aExpandP was split off.
-- (Most of it seems to have been removed now.)
-- (Also -ATS is a hidden flag, so this code isn't used much anyway.)

aExpandP :: APackage -> APackage
aExpandP apkg = 
	aRemoveUnused (apkg { apkg_local_defs = ds',
                              apkg_rules = rs',
                              apkg_interface = ifc' })
                                  
  where
    ds'  = map snd (M.toList globdm)
    rs'  = map (expR globsm) (apkg_rules apkg)
    ifc' = map (expI globsm) (apkg_interface apkg)

    -- def & subst map initial - start from back of dep list
    -- globdm has all 'ds' with expE applied
    -- globsm has i->e where i is substituteable
    (globdm, globsm) =
	    foldr expDef
		(M.empty, M.empty)
		(reverse (tsortADefs (apkg_local_defs apkg)))

    -- ----------
    -- expand in the actions of rules

    expR sm (ARule i ps d wp e as asmps o) = 
      ARule i ps d wp e (map (expA sm) as) (map (expAsmps sm) asmps) o

    expAsmps sm (AAssumption p as) = AAssumption p (map (expA sm) as)

    -- ----------
    -- expand in the definitions of the provided interface

    expI sm (AIDef mid is wp p (ADef i t e) fi asmps) =
	    AIDef mid is wp p (ADef i t (expE sm e)) fi (map (expAsmps sm) asmps)

    expI sm (AIAction is wp p i rs fi) =
	    AIAction is wp p i (map (expR sm) rs) fi

    expI sm (AIActionValue is wp p i rs (ADef iv t e) fi) =
	    AIActionValue is wp p i (map (expR sm) rs) (ADef iv t (expE sm e)) fi

    expI sm e@(AIClock {}) = e
    expI sm e@(AIReset {}) = e
    expI sm e@(AIInout {}) = e

    -- ----------
    -- expand expressions in actions

    expA sm (ACall i m es) = ACall i m (map (expE sm) es)
    expA sm (AFCall i f isC es isA) = AFCall i f isC (map (expE sm) es) isA
    expA sm (ATaskAction i f isC n es mid ty isA) = 
        ATaskAction i f isC n (map (expE sm) es) mid ty isA

    -- ----------
    -- expand AExpr

    -- expand sub-expressions
    expE sm (APrim aid t op es) = APrim aid t op (map (expE sm) es)
    expE sm (AMethCall t i m es) = AMethCall t i m (map (expE sm) es)
    expE sm (ANoInlineFunCall t i f es) = ANoInlineFunCall t i f (map (expE sm) es)
    expE sm (AFunCall t i f isC es) = AFunCall t i f isC (map (expE sm) es)

    -- if a local def reference is found in the "sm" map, then
    -- replace it with it's defined expression, otherwise leave as is
    expE sm (ASDef _ i) | me /= Nothing = e
	where
	    me = M.lookup i sm
	    Just e = me

    expE _ e = e

    -- ----------
    -- expand ADef

    -- turns a ADef _ _ e -> ADef _ _ (expE e)
    -- adds (i, (ADef i t (expE dsm e))) to dm
    expDef (ADef i t e) dsm@(dm, sm) =
	let e' = expE sm e
	    d' = ADef i t e'
	    sm' = M.insert i e' sm
	in  (M.insert i d' dm, sm')


-- ==============================


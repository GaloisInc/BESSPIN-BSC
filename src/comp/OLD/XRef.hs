-- #############################################################################
-- #
-- # (c) Copyright 2004, Bluespec Incorporated and Donald G. Baltus.
-- #
-- # Time-stamp: <2004-06-29 15:49:00 baltus>
-- #
-- # $Id$
-- # $Source:  $
-- #
-- #############################################################################

module XRef where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import FStringCompat
import List

import PPrint
import Id
import Position
import CSyntax
import Eval

-- #############################################################################
-- #
-- #############################################################################

data XInfo
	= XInfo {bsv_defs :: BSVDefMap,
		 cnode_list :: [String],
		 ifc_map :: [(String, Id)]} 


-- #############################################################################
-- #
-- #############################################################################

type BSVDefMap = M.Map String BSVDef

data BSVDef
	= BSVDef {bsv_package_name :: Id,
		  bsv_def_name :: Id,
		  verilog_def_name :: Id,
		  xsynthesized :: Bool,
		  bsv_children :: [BSVInst]}
	  deriving (Ord, Eq, Show)


data BSVInst
	= BSVInst {mod_inst_name :: Id,
		   mod_def :: BSVDef}
	  deriving (Ord, Eq, Show)

-- #############################################################################
-- #
-- #############################################################################

instance Hyper BSVDefMap where
    hyper x y = (x==x) `seq` y

instance Hyper BSVDef where
    hyper (BSVDef pi di vi xs bchildren) y = hyper5 pi di vi xs bchildren y

instance Hyper BSVInst where
    hyper (BSVInst mi md) y = hyper2 mi md y

-- #############################################################################
-- #
-- #############################################################################

instance PPrint BSVDef where
--    pPrint PDInfo p (BSVDef pid id _ _ []) = 
--	(text ("(bsv-inst \""  ++ (getIdString pid) ++ "\" \"" ++ (getIdString id) ++ "\")"))
--    pPrint PDInfo p (BSVDef pid id _ _ children) = 
--	(text ("(bsv-inst \""  ++ (getIdString pid) ++ "\" \"" ++ (getIdString id) ++ "\""))
--	$+$ (text "    (list") <+> (sep (map (pPrint PDInfo p) children)) <+> (text ")")
--	<+> (text ")")
    pPrint PDInfo p (BSVDef pid id _ _ children) = 
	(text ("bsv-inst \"TOP\" \"" ++ (getIdString id) ++ "\" "))
	$+$ (text "    [list") <+> (sep (map (pPrint PDInfo p) children)) <+> (text "]")
	<+> (text "")
    pPrint d p (BSVDef pid id _ _ []) = 
	(text ("[bsv-def \""  ++ (getIdString pid) ++ "\" \"" ++ (getIdString id) ++ "\"]"))
    pPrint d p (BSVDef pid id _ _ children) = 
	(text ("[bsv-def \""  ++ (getIdString pid) ++ "\" \"" ++ (getIdString id) ++ "\""))
	$+$ (text "    [list") <+> (sep (map (pPrint d p) children)) <+> (text "]")
	<+> (text "]")

-- #############################################################################
-- #
-- #############################################################################

instance PPrint BSVInst where
    pPrint PDInfo p (BSVInst id bsv_def@(BSVDef _ def_name verilog_name False [])) = 
	(text ("[bsv-inst \""  ++ (getIdString id) ++ "\" \"" ++ (getIdString def_name) ++ "\"]"))
    pPrint PDInfo p (BSVInst id bsv_def@(BSVDef _ def_name verilog_name False children)) = 
	(text ("[bsv-inst \""  ++ (getIdString id) ++ "\" \"" ++ (getIdString def_name) ++ "\""))
	$+$ (text "    [list") <+> (sep (map (pPrint PDInfo p) children)) <+> (text "]")
	<+> (text "]")
--    pPrint d p (BSVInst id (BSVDef _ def_name verilog_name False _)) = 
--	(text ("[bsv-inst \""  ++ (getIdString id) ++ "\" \""  ++ 
--	       (getIdString def_name) ++ "\"]"))
--    pPrint d p (BSVInst id (BSVDef _ def_name verilog_name True _)) = 
--	(text ("[verilog-inst \""  ++ (getIdString id) ++ "\" \""  ++ 
--	       (getIdString def_name) ++ "\"]"))
    pPrint d p (BSVInst id (BSVDef _ def_name verilog_name False _)) = 
	(text ("[bsv-inst \""  ++ (getIdString id) ++ "\" \"" ++ (getIdString def_name) ++ "\"]"))
    pPrint d p (BSVInst id (BSVDef _ def_name verilog_name True _)) = 
--	(text ("[verilog-inst \""  ++ (getIdString id) ++ "\"]"))
	(text ("[bsv-inst \""  ++ (getIdString id) ++ "\" \"" ++ (getIdString def_name) ++ "\"]"))

-- #############################################################################
-- #
-- #############################################################################

data PState = PState {
                      uniqueId :: Integer
                      }

type PPState a = State PState a

genPosition :: PPState Position
genPosition = do
        state <- get
        oldId <- gets uniqueId
        put state{ uniqueId = oldId + 1 }
        return $ (Position (mkFString "PositionLabel") (fromInteger oldId) 0 False False)

-- #############################################################################
-- #
-- #############################################################################

getIdLabel :: Id -> Int
getIdLabel id = case getIdProps id of
	[] -> 0
	[prop] -> getIdPropLabel prop
	props -> foldl1 (+) (map getIdPropLabel props)

getIdPropLabel :: IdProp -> Int
getIdPropLabel (IdP_positions []) = 0
getIdPropLabel (IdP_positions [pos]) = (getPositionLabel pos)
getIdPropLabel (IdP_positions pos_list) =
    (foldl1 (+) (map getPositionLabel pos_list))
getIdPropLabel prop = 0

getPositionLabel :: Position -> Int
getPositionLabel (Position fs label 0 _ _) =
    if (fs == (mkFString "PositionLabel")) then label else 0
getPositionLabel pos = 0

-- #############################################################################
-- #
-- #############################################################################

-- specialAdd :: (Integer,(S.Set OpNode)) -> OpMap -> OpMap
specialAdd (index, set) cmap =
    let set_current = (M.findWithDefault S.empty index cmap)
	set_new = (S.union set set_current)
	map_new = M.insert index set_new cmap
    in map_new

specialAdd2 (index, a) cmap =
    let set_current = (M.findWithDefault S.empty index cmap)
	set_new = (S.insert a set_current)
	map_new = M.insert index set_new cmap
    in map_new

--     in  hyper (M.size map_new) $ -- trace("INDEX " ++ (itos index) ++ " " 
-- 				   --	 ++ (concatMap createPositionString (map getOpNodePosition (S.toList set))) ++ " " 
-- 				   --	 ++ (concatMap createPositionString (map getOpNodePosition (S.toList set_current))) ++ " " 
-- 				   --	 ++ (concatMap createPositionString (map getOpNodePosition (S.toList set_new))) ++ "\n") $ 
--                                    map_new

-- #############################################################################
-- #
-- #############################################################################

-- specialUnion :: OpMap -> OpMap -> OpMap
specialUnion t1 t2 = if ((M.size t2) > (M.size t1))
		     then specialUnion' t2 (M.toList t1)
		     else specialUnion' t1 (M.toList t2)

--specialUnion :: OpMap -> OpMap -> OpMap
--specialUnion t1 t2 = trace("UNION " ++ (show (DS.cardinality (M.keysSet t1))) ++ " " ++ (show (DS.cardinality (M.keysSet t2)))) $
--                     specialUnion' t1 (M.toList t2)

specialUnion' t [] = t
specialUnion' t (xy:xys) = specialUnion' (specialAdd xy t) xys

-- #############################################################################
-- #
-- #############################################################################

createTopIfcMap :: CPackage -> [(String,Id)]
createTopIfcMap (CPackage _ _ _ _ def _) = 
    (concatMap createCDefnTopIfcMap def)

createCDefnTopIfcMap :: CDefn -> [(String,Id)]
createCDefnTopIfcMap (CValueSign (CDef id (CQType pred (TAp (TVar (TyVar _ n (KVar _)))           (TCon (TyCon id_ifc _ _))       )) _)) = [((getIdString id), id_ifc)]
createCDefnTopIfcMap (CValueSign (CDef id (CQType pred (TAp (TVar (TyVar _ n (KVar _)))      (TAp (TCon (TyCon id_ifc _ _)) _)    )) _)) = [((getIdString id), id_ifc)]
createCDefnTopIfcMap (CValueSign (CDef id (CQType pred (TAp (TVar (TyVar _ n (KVar _))) (TAp (TAp (TCon (TyCon id_ifc _ _)) _) _) )) _)) = [((getIdString id), id_ifc)]
createCDefnTopIfcMap cdefn = []

-- #############################################################################
-- #
-- #############################################################################

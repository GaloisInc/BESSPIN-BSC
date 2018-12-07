module SceMiElement (
    SceMiLinkType(..),
    SceMiElement(..),
    getSceMiMap,
    findSceMiElements,
    fmtSceMiEl
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe(fromMaybe, isJust, isNothing, listToMaybe)
import Data.List(intersperse, isPrefixOf, isSuffixOf)
import Control.Monad(mplus, guard)

import Util(headOrErr)
import ListUtil(splitBy)

import ErrorUtil(internalError)
import PPrint
import PVPrint
import Flags(Flags(..))
import Id( Id, getIdString, getIdQualString
         , getIdBaseString, setIdBaseString
         )
import IntLit
import CType(CType,leftCon,tyConArgs)
import SymTab(SymTab(..))
import ASyntax
import ASyntaxUtil(isConst, isASAny)
import InstNodes(InstNode(..))
import ABin(ABinModInfo(..))
import ABinUtil(HierMap, InstModMap)
import TypeAnalysis(analyzeType, getWidth)

-- -------------------------------------------------------------------
-- Structures for describing Sce-Mi elements

-- This should match the SceMiLinkType definition in
-- src/lib/BSVSource/SceMi/SceMiDefines.bsv
data SceMiLinkType = TCP
                   | SCEMI
                   | EVE
                   | ALDEC
                   | PCIE_VIRTEX5
                   | PCIE_VIRTEX6
                   | PCIE_KINTEX7
                   | PCIE_VIRTEX7
                   | PCIE_VIRTEXU
                   | PCIE_DINI
		   | PCIE_ARRIA10
  deriving (Eq, Ord, Show);

convertLinkTypeFromBSV :: Integer -> SceMiLinkType
convertLinkTypeFromBSV 0 = TCP
convertLinkTypeFromBSV 1 = SCEMI
convertLinkTypeFromBSV 2 = EVE
convertLinkTypeFromBSV 3 = ALDEC
convertLinkTypeFromBSV 4 = PCIE_VIRTEX5
convertLinkTypeFromBSV 5 = PCIE_VIRTEX6
convertLinkTypeFromBSV 6 = PCIE_KINTEX7
convertLinkTypeFromBSV 7 = PCIE_VIRTEX7
convertLinkTypeFromBSV 8 = PCIE_VIRTEXU
convertLinkTypeFromBSV 9 = PCIE_DINI
convertLinkTypeFromBSV 10= PCIE_ARRIA10
convertLinkTypeFromBSV n = internalError $ "invalid link type constant: " ++ (show n)

-- Structure describing each type of standard Sce-Mi element (and our extras):
data SceMiElement = SceMiInPort { xactor    :: String
                                , port_name :: String
                                , port_size :: Integer
                                , port_type :: String
                                , link_type :: SceMiLinkType
                                , chan_num  :: Maybe Integer
                                }
                  | SceMiOutPort { xactor    :: String
                                 , port_name :: String
                                 , port_size :: Integer
                                 , port_type :: String
                                 , link_type :: SceMiLinkType
                                 , chan_num  :: Maybe Integer
                                 }
                  | SceMiClock { clk_name         :: String
                               , clk_num          :: Integer
                               , clk_group        :: Maybe Integer
                               , clk_numerator    :: Integer
                               , clk_denominator  :: Integer
                               , clk_duty_hi      :: Integer
                               , clk_duty_lo      :: Integer
                               , clk_phase        :: Integer
                               , clk_reset_cycles :: Integer
                               , link_type        :: SceMiLinkType
                               }
                  | SceMiClockBinding { xactor    :: String
                                      , clk_num   :: Integer
                                      , link_type :: SceMiLinkType
                                      }
                  | SceMiInputPipe { xactor          :: String
                                   , pipe_name       :: String
                                   , pipe_size       :: Integer
                                   , pipe_type       :: String
                                   , pipe_depth      :: Integer
                                   , pipe_visibility :: String
                                   , link_type       :: SceMiLinkType
                                   , pipe_num        :: Integer
                                   }
                  | SceMiOutputPipe { xactor          :: String
                                    , pipe_name       :: String
                                    , pipe_size       :: Integer
                                    , pipe_type       :: String
                                    , pipe_depth      :: Integer
                                    , pipe_visibility :: String
                                    , link_type       :: SceMiLinkType
                                    , pipe_num        :: Integer
                                    }
                  | SceMiSerial { ser_path    :: String
                                , ser_num     :: Integer
                                , ser_label   :: String
                                , ser_kind    :: Integer
                                , ser_samples :: Integer
                                , ser_offset  :: Integer
                                , ser_width   :: Integer
                                , ser_type    :: String
                                }
  deriving (Eq,Ord,Show)

data SceMiElementType = MessageInPort
                      | MessageOutPort
                      | InputPipe
                      | OutputPipe
                      | ClockPort
                      | ClockController
                      | SerialInfo
  deriving (Eq,Ord,Show)

-- -------------------------------------------------------------------

getSceMiMap :: [(String, (ABinModInfo, String))] -> M.Map String [([Id], CType)]
getSceMiMap abmis_by_name =
    M.fromList [ (mod, concat subs)
                 | (mod,(abmi,_)) <- abmis_by_name
                 , let pkg = abmi_apkg abmi
                 , let subs = [ submods
                                | inst_node <- M.elems (apkg_inst_tree pkg)
                                , let submods = getSceMiData [] inst_node
                                , not (null submods)
                              ]
               ]

-- Walk the InstTree data and identify modules that match known
-- SceMi module types for clock ports, message ports, etc.
getSceMiData :: [Id] -> InstNode -> [([Id],CType)]
getSceMiData _ (StateVar {}) = []
getSceMiData _ (Rule {})     = []
getSceMiData p l@(Loc {})    =
    let name        = find_name ((node_name l):p) l
        mt          = node_type l
        rest        = concatMap (getSceMiData name) (M.elems (node_children l))
    in maybe rest (:rest) ((normal_module name mt) `mplus` (scemi_module name mt))
    where -- process type in the normal Module monad
          normal_module nm mt = do ctype <- mt
                                   con <- leftCon ctype
                                   guard (getIdQualString con == "SceMiDefines")
                                   guard (isSceMiElement ctype)
                                   return (nm,ctype)
          -- process type in [the SceMiModule] a ModuleContext monad
          scemi_module nm mt = do ctype <- mt
                                  con <- leftCon ctype
                                  guard (getIdString con == "Prelude.PrimPair")
                                  let [_,arg2] = tyConArgs ctype
                                  --con1 <- leftCon arg1
                                  --guard (getIdString con1 == "SceMiInternals.SceMiModuleState")
                                  normal_module nm (Just arg2)

          -- figure out the evaluator name for this node, if possible,
          -- based on state instances below this node.  This is required
          -- because the evaluator may choose a different name than the
          -- InstTree (for vectors, etc.) at non-synthesized boundaries.
          find_name nm l =
              let base = filter (/='.') $ joinStrings "_" (map getIdString nm)
                  lt_params = get_link_params (node_children l)
              in case lt_params of
                   []  -> nm
                   (i:_) -> [setIdBaseString i (fix_name base (getIdBaseString i))]
          -- lookup param_link_type state instances below this point in
          -- the InstTree
          get_link_params m =
              concat [ case node of
                         (Loc {})       -> get_link_params (node_children node)
                         (StateVar aid) -> if ("param_link_type" `isSuffixOf` (getIdBaseString aid)) ||
                                              ("param_prbnum"    `isSuffixOf` (getIdBaseString aid))
                                           then [aid]
                                           else []
                         otherwise      -> []
                     | node <- M.elems m
                     ]
          -- use a synthesized name to fixup the InstTree-based name
          -- by dropping elements from the InstTree name which don't
          -- have corresponding elements in the synthesized name.
          fix_name n1 n2 =
              let s1 = splitBy (== '_') n1
                  s2 = splitBy (== '_') n2
              in joinStrings "_" (reverse (dropExtras s1 s2))
          -- drop elements of the first list which don't exist in
          -- the second list.
          dropExtras [] ys = []
          dropExtras xs [] = []
          dropExtras (x:xs) l@(y:ys)
              | x == y    = x:(dropExtras xs ys)
              | otherwise = dropExtras xs l

-- Is this the type of a SceMi clock port?
isClockPort :: CType -> Bool
isClockPort t = case (leftCon t) of
                  (Just i) -> getIdBaseString i == "SceMiClockPortIfc"
                  Nothing  -> False

-- Is this the type of a SceMi clock controller?
isClockControl :: CType -> Bool
isClockControl t = case (leftCon t) of
                     (Just i) -> getIdBaseString i == "SceMiClockControlIfc"
                     Nothing  -> False

-- Is this the type of a SceMi message input port?
isMessageInPort :: CType -> Bool
isMessageInPort t = case (leftCon t) of
                      (Just i) -> getIdBaseString i == "SceMiMessageInPortIfc"
                      Nothing  -> False

-- Is this the type of a SceMi message output port?
isMessageOutPort :: CType -> Bool
isMessageOutPort t = case (leftCon t) of
                       (Just i) -> getIdBaseString i == "SceMiMessageOutPortIfc"
                       Nothing  -> False

-- Is this the type of a SceMi message port in either direction?
isMessagePort :: CType -> Bool
isMessagePort t = (isMessageInPort t) || (isMessageOutPort t)

-- Is this the type of a SceMi message input pipe?
isInputPipe :: CType -> Bool
isInputPipe t = case (leftCon t) of
                  (Just i) -> getIdBaseString i == "SceMiInputPipeIfc"
                  Nothing  -> False

-- Is this the type of a SceMi message output pipe?
isOutputPipe :: CType -> Bool
isOutputPipe t = case (leftCon t) of
                   (Just i) -> getIdBaseString i == "SceMiOutputPipeIfc"
                   Nothing  -> False

-- Is this the type of a SceMi pipe in either direction?
isPipe :: CType -> Bool
isPipe t = (isInputPipe t) || (isOutputPipe t)

-- Is this the type of a SceMi serial info interface?
isSerialInfo :: CType -> Bool
isSerialInfo t = case (leftCon t) of
                   (Just i) -> getIdBaseString i == "SceMiSerialInfo"
                   Nothing  -> False

-- Convert a CType into its corresponding SceMiElementType
elementType :: CType -> Maybe SceMiElementType
elementType t = listToMaybe [ et | (p,et) <- mapping, p t ]
  where mapping = [ (isClockPort,      ClockPort)
                  , (isClockControl,   ClockController)
                  , (isMessageInPort,  MessageInPort)
                  , (isMessageOutPort, MessageOutPort)
                  , (isInputPipe,      InputPipe)
                  , (isOutputPipe,     OutputPipe)
                  , (isSerialInfo,     SerialInfo)
                  ]

-- Is this a SCE-MI element?
isSceMiElement :: CType -> Bool
isSceMiElement t = isJust (elementType t)

-- -------------------------------------------------------------------

type PkgMap = M.Map String APackage

-- Convert a path and type from getSceMiData into a SceMiElement structure.
-- The xactor and relative names are computed, the types are analyzed to
-- determine message port widths and information is extracted from module
-- parameters (which have been explicitly recorded in the library source).
mkSceMiElement :: Flags -> SymTab -> InstModMap -> PkgMap ->
                  Maybe [String] -> [String] -> [String] -> ([Id],CType) -> SceMiElement
mkSceMiElement flags symtab instmap pkgmap mxact path fullpath (name,ty) =
    let -- determine name
        xactor = case mxact of
                   (Just xact) -> joinStrings "." xact
                   Nothing     -> ""
        el_name = joinStrings "_" ((map getIdString name) ++ path)
        full_el_name = joinStrings "_" ((map getIdString name) ++ fullpath)
        -- extract module parameters
        avis   = fromMaybe [] $ do mod <- M.lookup xactor instmap
                                   pkg <- M.lookup mod pkgmap
                                   return (apkg_state_instances pkg)
        pfx    =  if (null el_name)
                    then "param_"
                    else el_name ++ "_param_"
        params = [ (param, val)
                 | avi <- avis
                 , let name = getIdString (avi_vname avi)
                 , pfx `isPrefixOf` name
                 , let param = drop (length pfx) name
                 , let pexpr = headOrErr "missing SceMi parameter" (getParams avi)
                 , (isConst pexpr) && (not (isASAny pexpr))
                 , let val = ilValue (ae_ival pexpr)
                 ]
        lookupParam p = case (lookup p params) of
                          (Just v) -> v
                          Nothing  -> internalError $ "unable to find SceMi parameter " ++ p
        isStr :: AExpr -> Bool
        isStr (ASStr {}) = True
        isStr _ = False
        strParams = [ (param, val)
                    | avi <- avis
                    , let name = getIdString (avi_vname avi)
                    , pfx `isPrefixOf` name
                    , let param = drop (length pfx) name
                    , let pexpr = headOrErr "missing SceMi parameter" (getParams avi)
                    , (isStr pexpr)
                    , let val = (ae_strval pexpr)
                    ]
        lookupStrParam p = case (lookup p strParams) of
                             (Just v) -> v
                             Nothing  -> internalError $ "unable to find SceMi parameter " ++ p
        -- find the port type info
        get_port_size t = let msg_type = headOrErr "no type argument found for SceMi message port" (tyConArgs t)
                              ta = analyzeType flags symtab msg_type
                          in fromMaybe 0 (either (const Nothing) getWidth ta)
        get_port_type_desc t = let msg_type = headOrErr "no type argument found for SceMi message port" (tyConArgs t)
                               in pvpString msg_type
        -- find the pipe type info
        get_pipe_size t = let ts = tyConArgs t
                          in case ts of
                               [_,msg_type] -> let ta = analyzeType flags symtab msg_type
                                               in fromMaybe 0 (either (const Nothing) getWidth ta)
                               _ -> internalError "no type argument found for SceMi pipe"
        get_pipe_type_desc t = let ts = tyConArgs t
                               in case ts of
                                    [_,msg_type] -> pvpString msg_type
                                    _            -> internalError "no type argument found for SceMi pipe"
        -- extract the linkage type
        lt = convertLinkTypeFromBSV $ lookupParam "link_type"
        -- extract the clock group parameter, if it exists
        clock_group = do cg <- lookup "clockGroup" params
                         guard (cg /= (2^(32::Integer)-1))
                         return cg
        remove_spaces s = let notspace c = (c /= ' ')
                          in filter notspace s
        lfen = length full_el_name
        serial_path = take (lfen - 7) full_el_name
    in if (not (isClockPort ty) && (isNothing mxact))
       then internalError $ "mkSceMiElement with no transactor: " ++ (ppReadable (name,ty))
       else case (elementType ty) of
              (Just MessageInPort)  -> SceMiInPort { xactor    = xactor
                                                   , port_name = el_name
                                                   , port_size = get_port_size ty
                                                   , port_type = remove_spaces (get_port_type_desc ty)
                                                   , link_type = lt
                                                   , chan_num  = lookup "channelId" params
                                                   }
              (Just MessageOutPort) -> SceMiOutPort { xactor    = xactor
                                                    , port_name = el_name
                                                    , port_size = get_port_size ty
                                                    , port_type = remove_spaces (get_port_type_desc ty)
                                                    , link_type = lt
                                                    , chan_num  = lookup "channelId" params
                                                    }
              (Just InputPipe)      -> SceMiInputPipe { xactor          = xactor
                                                      , pipe_name       = el_name
                                                      , pipe_size       = get_pipe_size ty
                                                      , pipe_type       = remove_spaces (get_pipe_type_desc ty)
                                                      , pipe_depth      = lookupParam "depth"
                                                      , pipe_visibility = lookupStrParam "visibility"
                                                      , link_type       = lt
                                                      , pipe_num        = lookupParam "pipeNum"
                                                      }
              (Just OutputPipe)     -> SceMiOutputPipe { xactor          = xactor
                                                       , pipe_name       = el_name
                                                       , pipe_size       = get_pipe_size ty
                                                       , pipe_type       = remove_spaces (get_pipe_type_desc ty)
                                                       , pipe_depth      = lookupParam "depth"
                                                       , pipe_visibility = lookupStrParam "visibility"
                                                       , link_type       = lt
                                                       , pipe_num        = lookupParam "pipeNum"
                                                       }
              (Just ClockPort)      -> SceMiClock { clk_name         = joinStrings "." [el_name,xactor]
                                                  , clk_num          = lookupParam "clockNum"
                                                  , clk_group        = clock_group
                                                  , clk_numerator    = lookupParam "ratioNum"
                                                  , clk_denominator  = lookupParam "ratioDen"
                                                  , clk_duty_hi      = lookupParam "dutyHi"
                                                  , clk_duty_lo      = lookupParam "dutyLo"
                                                  , clk_phase        = lookupParam "phase"
                                                  , clk_reset_cycles = lookupParam "rstStage"
                                                  , link_type        = lt
                                                  }
              (Just ClockController)  -> SceMiClockBinding { xactor    = xactor
                                                           , clk_num   = lookupParam "clockNum"
                                                           , link_type = lt
                                                           }
              (Just SerialInfo)       -> let knd = lookupParam "prbknd"
                                         in SceMiSerial  { ser_path    = serial_path
                                                         , ser_num     = lookupParam "prbnum"
                                                         , ser_label   = lookupStrParam "label"
                                                         , ser_kind    = knd
                                                         , ser_samples = case (knd) of
                                                                           1 -> lookupParam "numsamples"
                                                                           3 -> lookupParam "numsamples"
                                                                           _  -> 0
                                                         , ser_offset  = case (knd) of
                                                                           1 -> lookupParam "offset"
                                                                           3 -> lookupParam "offset"
                                                                           _ -> 0
                                                         , ser_width   = lookupParam "width"
                                                         , ser_type    = remove_spaces (get_port_type_desc ty)
                                                         }
              _ -> internalError $ "mkSceMiElement with unknown type: " ++ (ppReadable (name,ty))

-- Build a set of all SceMiElements in the entire design
findSceMiElements :: Flags -> SymTab -> (String,String) ->
                     (Maybe [String],[String]) ->
                     HierMap -> InstModMap -> PkgMap ->
                     M.Map String [([Id],CType)] -> S.Set SceMiElement
findSceMiElements flags symtab (inst,mod) (xact,path) hiermap instmap pkgmap scemimap =
    let els = -- trace ("find: " ++ show(inst,mod) ++ "; " ++ show(xact,path) ++ ";\n")
              (M.findWithDefault [] mod scemimap)
        has_clock_ctrl = any (isClockControl . snd) els
        has_serial     = any (isSerialInfo . snd) els
        has_port       = any (isMessagePort . snd) els
        has_pipe       = any (isPipe . snd) els
        is_xactor = has_clock_ctrl || has_serial || ((has_port || has_pipe) && isNothing xact)
        (xact',path') = if is_xactor
                        then case xact of
                               (Just p) -> (Just (inst:path ++ p), [])
                               Nothing  -> (Just (inst:path), [])
                        else (xact,inst:path)
        subs = M.findWithDefault ([],[]) mod hiermap
        local_els = S.fromList $ map (mkSceMiElement flags symtab instmap pkgmap xact' path' (inst:path))
                                     els
        sub_els = [ findSceMiElements flags
                                      symtab
                                      (new_inst,new_mod)
                                      (xact',path')
                                      hiermap
                                      instmap
                                      pkgmap
                                      scemimap
                  | (new_inst,new_mod) <- (fst subs) ++ (snd subs)
                  ]
    in S.unions (local_els:sub_els)

-- -------------------------------------------------------------------

-- Pretty-print a SceMiElement
fmtSceMiEl :: SceMiElement -> String
fmtSceMiEl el@(SceMiInPort {}) =
    let name = joinStrings "." [port_name el,xactor el]
        ty   = case (chan_num el) of
                 Just n  -> "SceMiMessageInPort (Channel " ++ (show n) ++ ")"
                 Nothing -> "SceMiMessageInPort"
    in name ++ " " ++ (show (port_size el)) ++ "-bit " ++ ty
fmtSceMiEl el@(SceMiOutPort {}) =
    let name = joinStrings "." [port_name el,xactor el]
        ty   = case (chan_num el) of
                 Just n  -> "SceMiMessageOutPort (Channel " ++ (show n) ++ ")"
                 Nothing -> "SceMiMessageOutPort"
    in name ++ " " ++ (show (port_size el)) ++ "-bit " ++ ty
fmtSceMiEl el@(SceMiInputPipe {}) =
    let name = joinStrings "." [pipe_name el,xactor el]
        ty   = "SceMiInputPipe (Pipe# " ++ (show (pipe_num el)) ++ ")"
    in name ++ " " ++ (show (pipe_size el)) ++ "-bit " ++ ty
fmtSceMiEl el@(SceMiOutputPipe {}) =
    let name = joinStrings "." [pipe_name el,xactor el]
        ty   = "SceMiOutputPipe (Pipe# " ++ (show (pipe_num el)) ++ ")"
    in name ++ " " ++ (show (pipe_size el)) ++ "-bit " ++ ty
fmtSceMiEl el@(SceMiClock {}) =
    case (clk_group el) of
      Just n  -> (clk_name el) ++ " SceMiClockPort #" ++ (show (clk_num el)) ++ " (Group " ++ (show n) ++ ")"
      Nothing -> (clk_name el) ++ " SceMiClockPort #" ++ (show (clk_num el))
fmtSceMiEl el@(SceMiClockBinding {}) =
    (xactor el) ++ " SceMiClockControl for clock #" ++ (show (clk_num el))
fmtSceMiEl el@(SceMiSerial {}) =
    (ser_label el) ++ " SceMiSerial #" ++ (show (ser_num el))

-- -------------------------------------------------------------------
-- Utility functions

-- Note: This assumes that list needs to be reversed!
joinStrings :: String -> [String] -> String
joinStrings sep ss =
    let ss' = [ s | s <- ss, not (null s), (head s) /= '_' ]
    in  concat (intersperse sep (reverse ss'))

-- -------------------------------------------------------------------

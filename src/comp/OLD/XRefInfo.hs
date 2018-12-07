module XRefInfo(addMarkerFile, addMarkerPositions, writeCrossInfo, createLinkInfoString, controlifyString, extractCrossInfo, removeTabs) where

import Char(isDigit, digitToInt)
import FStringCompat(mkFString, getFString)
import Id(Id,getIdString,setIdBase,getIdPosition,setIdPosition, mkId)
import Maybe(isNothing, fromJust)
import PPrint(ppr, ppReadable, text, PDetail(..), PPrint(..))
import Position(Position(..),
	getPositionFile, getPositionLine, getPositionColumn,
	mkPosition, noPosition, updatePosLineCol, updatePosFile)
import Util(fromJustOrErr, getEnvDef)
import FileNameUtil(getFullFilePath, removeQuotes)
import FileIOUtil(writeFileCatch)
import XRef(BSVInst(..),BSVDef(..), BSVDefMap)
import List(nub)
import qualified Data.Map as M
import Version(versionnum)
import BuildVersion(buildVersion)
import ErrorUtil(internalError)

-- #############################################################################
-- #
-- #############################################################################

data Marker
	= Marker String         -- Module name.
	         Id             -- Name and position in verilog file.
                 Position       -- Position in BSV file
                 String         -- kind of identifier
                 (Maybe String) -- Possible argument
	  deriving (Ord, Eq, Show)

-- #############################################################################
-- #
-- #############################################################################

dfltBluespecDir = "/"

-- #############################################################################
-- #
-- #############################################################################

infoSpaceChar = controlifyChar ' '
infoSpaceString = [infoSpaceChar]

-- #############################################################################
-- #
-- #############################################################################

extractCrossInfo :: String -> [Marker] -> [[Marker]]
extractCrossInfo "" current = [current]
extractCrossInfo string current = 
    let start = takeWhile (/= '<') string
        rest = dropWhile (/= '<') string
        prefix = take 8 rest
    in if (prefix == ("<<<info" ++ infoSpaceString))
       then let info = (takeWhile (/= '>') (drop 8 rest))
            in if ((length start) == 0)
	       then (extractCrossInfo (drop ((length info) + 8 + 3) rest) ((extractMarker info) : current))
	       else (current : (extractCrossInfo (drop ((length info) + 8 + 3) rest) [(extractMarker info)]))
       else (extractCrossInfo (drop 1 rest) current)

-- #############################################################################
-- #
-- #############################################################################

extractMarker :: String -> Marker
extractMarker string = 
               let name = (nthWord infoSpaceChar 0 string)
		   fileName = (drop 2 (nthWord infoSpaceChar 1 string))
		   line = (stringToInt 10 (nthWord infoSpaceChar 2 string))
		   column = (stringToInt 10 (nthWord infoSpaceChar 3 string))
		   label = (nthWord ' ' 0 (nthWord infoSpaceChar 4 string))
		   arg = (nthWord ' ' 1 (nthWord infoSpaceChar 4 string))
		in if (arg /= "") 
		   then (Marker
			    "XX"
			    (mkId noPosition (mkFString name))
			    (mkPosition (mkFString fileName) line column)
			    label
			    (Just arg))
		   else (Marker
			    "XX"
			    (mkId noPosition (mkFString name))
			    (mkPosition (mkFString fileName) line column)
			    label
			    Nothing)


-- #############################################################################
-- #
-- #############################################################################

addMarkerPositions :: Int -> Int -> String -> [[Marker]] -> [[Marker]]
addMarkerPositions line column string [] = []
addMarkerPositions line column string ( [] : rest ) = (addMarkerPositions line column string rest)
addMarkerPositions line column "" marker_list = marker_list
addMarkerPositions line column (char : string_rest) ( markers : rest) =
    if (char == '\n') 
    then (addMarkerPositions (line + 1) 1 string_rest (markers : rest))
    else if (isControlifiedChar char)
	 then let marker_list = (map (addMarkerPosition line column) markers)
	      in (marker_list : (addMarkerPositions line (column + 1) string_rest rest))
	 else (addMarkerPositions line (column + 1) string_rest (markers : rest))

-- #############################################################################
-- #
-- #############################################################################

addMarkerFile :: String -> String -> Marker -> Marker
addMarkerFile mod_name file_name (Marker _ id pos label arg) =
    (Marker mod_name (setIdPosition (updatePosFile (getIdPosition id) (mkFString file_name) ) id) pos label arg)

-- #############################################################################
-- #
-- #############################################################################

addMarkerPosition :: Int -> Int -> Marker -> Marker
addMarkerPosition line column (Marker mod_name id pos label arg) =
    (Marker mod_name (setIdPosition (updatePosLineCol (getIdPosition id) line column) id) pos label arg)

-- #############################################################################
-- #
-- #############################################################################

instance PPrint Marker where
    pPrint d p (Marker mod_name id (Position file line column _ _) label arg) =
     let
       (Position file_v line_v column_v _ _) = getIdPosition id
     in
	if (isNothing arg)
	then text ("add-bsv-id-info " ++
		   "\"" ++ mod_name            ++ "\" " ++
		   "{" ++ (getIdString id)    ++ "} " ++
		   "\"" ++ (getFString file_v) ++ "\" " ++
		   ""   ++ (show line_v)       ++ " " ++
		   ""   ++ (show column_v)     ++ " " ++
		   "\"" ++ (removeQuotes (getFullFilePath (getFString file))) ++ "\" " ++
		   ""   ++ (show line)         ++ " " ++
		   ""   ++ (show column)       ++ " " ++
		   "\"" ++ label               ++ "\" " ++
		   "")
	else text ("add-bsv-id-info " ++
		   "\"" ++ mod_name            ++ "\" " ++
		   "{" ++ (getIdString id)    ++ "} " ++
		   "\"" ++ (getFString file_v) ++ "\" " ++
		   ""   ++ (show line_v)       ++ " " ++
		   ""   ++ (show column_v)     ++ " " ++
		   "\"" ++ (removeQuotes (getFullFilePath (getFString file))) ++ "\" " ++
		   ""   ++ (show line)         ++ " " ++
		   ""   ++ (show column)       ++ " " ++
		   "\"" ++ label               ++ "\" " ++
		   "\"" ++ (fromJustOrErr "XRefInfo.pPrint (Marker)" arg) ++ "\" " ++
		   "")

-- #############################################################################
-- #
-- #############################################################################

writeCrossInfo :: String -> FilePath -> [String] -> BSVDefMap -> [Marker] -> IO ()
writeCrossInfo modName fileName blurb bsv_def_map marker_list =
    let maybe_top_def = M.lookup modName bsv_def_map
    in if (isNothing maybe_top_def)
       then do
	      bdir <- getEnvDef "BLUESPECDIR" dfltBluespecDir
              writeFileCatch fileName  "### -*- Tcl -*- ################################################################\n"
	      appendFile fileName (commentL blurb)
	      appendFile fileName "################################################################################\n\n"
	      appendFile fileName "\n\n"
	      appendFile fileName ("compiler-version \"" ++ versionnum ++"\"\n")
	      appendFile fileName ("build-version \"" ++ buildVersion ++"\"\n")
	      appendFile fileName ("bluespec-dir \"" ++ bdir ++"\"\n\n")
	      appendFile fileName ("set-hierarchy {\n")
	      appendFile fileName "bsv-inst \"TOP\" [list]\n"
	      appendFile fileName ("}\n")
--	      appendFile fileName (concatMap ppReadable (M.elems bsv_def_map))
	      appendFile fileName "\n\n"
	      appendFile fileName (concatMap ppReadable marker_list)
       else let top_def = (fromJust maybe_top_def) { xsynthesized = False }
	        def_names = (collectBSVDefNames top_def)
	    in do
	         bdir <- getEnvDef "BLUESPECDIR" dfltBluespecDir
                 writeFileCatch fileName  "### -*- Tcl -*- ################################################################\n"
		 appendFile fileName (commentL blurb)
		 appendFile fileName "################################################################################\n\n"
		 appendFile fileName "\n\n"
		 appendFile fileName ("compiler-version \"" ++ versionnum ++"\"\n")
		 appendFile fileName ("build-version \"" ++ buildVersion ++"\"\n")
		 appendFile fileName ("bluespec-dir \"" ++ bdir ++"\"\n\n")
	         appendFile fileName ("set-hierarchy {\n")
		 appendFile fileName (ppr PDInfo top_def)
	         appendFile fileName ("}\n")
--		 appendFile fileName (concatMap ppReadable (M.elems bsv_def_map))
		 appendFile fileName "\n\n"
		 appendFile fileName (concatMap (ppr PDInfo) (concatMap (createBSVDefDEFOFMarkers modName "" def_names) 
							      (M.elems bsv_def_map)))
		 appendFile fileName "\n"
		 appendFile fileName (concatMap (ppr PDInfo) (concatMap (createBSVDefINSTOFMarkers modName "" def_names) 
							      [top_def]))
		 appendFile fileName "\n\n"
		 appendFile fileName (concatMap ppReadable marker_list)

commentL :: [String] -> String
commentL ls = unlines (["##"] ++ map ("## " ++) ls ++ ["##"])

-- #############################################################################
-- # Only negative number this needs to know about is -1.
-- #############################################################################

stringToInt :: Int -> String -> Int
stringToInt 10 "-1" = -1
stringToInt radix s
    | all isDigit s = foldl1 (\n d -> n * radix + d) (map digitToInt s)
    | otherwise = internalError ("XRefInfo.stringToInt: " ++ show s)

-- #############################################################################
-- #
-- #############################################################################

nthWord :: Char -> Int -> String -> String
nthWord char n "" = ""
nthWord char n string =
    if (n == 0)
    then (takeWhile (/= char) (dropWhile (== char) string))
    else (nthWord char (n - 1) (dropWhile (/= char) (dropWhile (== char) string)))

-- #############################################################################
-- #
-- #############################################################################

removeTabs :: String -> String
removeTabs "" = ""
removeTabs xstring =
    let z0 = takeWhile (/= '\t') xstring
	z1 = dropWhile (/= '\t') xstring
    in z0 ++ "        " ++ (removeTabs (drop 1 z1))

-- #############################################################################
-- #
-- #############################################################################

createLinkInfoString :: String -> String -> Position -> String
createLinkInfoString linkType label position = 
    let body = (label ++ infoSpaceString ++ (createPositionInfoString position) ++ infoSpaceString ++ linkType)
    in createInfoString body

createPositionInfoString :: Position -> String
createPositionInfoString position = 
    let body = (infoSpaceString ++ "F:" ++ (getPositionFile position) ++ infoSpaceString 
		      ++ (show (getPositionLine position)) ++ infoSpaceString 
		      ++ (show (getPositionColumn position)))
    in body

-- #############################################################################
-- #
-- #############################################################################

createInfoString :: String -> String
createInfoString string = ("<<<info" ++ infoSpaceString ++ string ++ infoSpaceString ++ ">>>")

-- #############################################################################
-- #
-- #############################################################################

controlifyString :: String -> String
controlifyString [] = []
controlifyString (a:rest) = (controlifyChar a) : rest

controlifyChar :: Char -> Char
controlifyChar char = (toEnum ((fromEnum char) + 128))

-- unControlifyChar :: Char -> Char
-- unControlifyChar char = (toEnum ((fromEnum char) - 128))

-- isControlifiedString :: String -> Bool
-- isControlifiedString [] = False
-- isControlifiedString (a:rest) = (isControlifiedChar a)

isControlifiedChar :: Char -> Bool
isControlifiedChar char = (fromEnum char) >= 128

-- #############################################################################
-- #
-- #############################################################################

createBSVDefINSTOFMarkers :: String -> String -> [String] -> BSVDef -> [Marker]
createBSVDefINSTOFMarkers modName prefix def_names (BSVDef _ _ _ True children) = []

createBSVDefINSTOFMarkers modName prefix def_names (BSVDef _ def_id _ False children) =
    if (elem (getIdString def_id) def_names)
    then (concatMap (createBSVInstINSTOFMarkers modName prefix def_names) children)
    else []

createBSVInstINSTOFMarkers :: String -> String -> [String] -> BSVInst -> [Marker]
createBSVInstINSTOFMarkers modName prefix def_names (BSVInst id bsv_def@(BSVDef _ def_id verilog_def_id _ children)) =
         let modifyId "" oid = oid
	     modifyId prefix oid =
		 (setIdBase oid (mkFString (prefix ++ "." ++ (getIdString oid))))
	     new_id = modifyId prefix id
	     marker = (Marker modName
		       (setIdPosition noPosition new_id)
		       (getIdPosition new_id)
		       "BINSTOF" (Just (getIdString def_id)))
	 in (marker : (createBSVDefINSTOFMarkers modName (getIdString new_id) def_names bsv_def))


-- #############################################################################
-- #
-- #############################################################################


createBSVDefDEFOFMarkers :: String -> String -> [String] -> BSVDef -> [Marker]
createBSVDefDEFOFMarkers modName prefix def_names (BSVDef _ _ _ True children) = []

createBSVDefDEFOFMarkers modName prefix def_names (BSVDef _ def_id _ False children) =
    if (prefix == "") && (not (modName == (getIdString def_id))) && (elem (getIdString def_id) def_names)
    then
      let modifyId "" oid = oid
	  modifyId prefix oid =
	      (setIdBase oid (mkFString (prefix ++ "." ++ (getIdString oid))))
	  new_id = modifyId prefix def_id
	  marker = (Marker modName
		    (setIdPosition noPosition new_id)
		    (getIdPosition new_id)
		    "FLATTENED_DEFOF" (Just (getIdString def_id)))
      in (marker : (concatMap (createBSVInstDEFOFMarkers modName prefix def_names) children))
    else if (elem (getIdString def_id) def_names)
    then (concatMap (createBSVInstDEFOFMarkers modName prefix def_names) children)
    else []

createBSVInstDEFOFMarkers :: String -> String -> [String] -> BSVInst -> [Marker]
createBSVInstDEFOFMarkers modName prefix def_names (BSVInst _ (BSVDef _ def_id verilog_def_id True children)) = []
createBSVInstDEFOFMarkers modName prefix def_names (BSVInst id bsv_def@(BSVDef _ def_id verilog_def_id False children)) =
         let modifyId "" oid = oid
	     modifyId prefix oid =
		 (setIdBase oid (mkFString (prefix ++ "." ++ (getIdString oid))))
	     new_id = modifyId prefix id
	 in (createBSVDefDEFOFMarkers modName (getIdString new_id) def_names bsv_def)

-- #############################################################################
-- #
-- #############################################################################

collectBSVDefNames (BSVDef _ def_id _ _ children) = 
    let getBSVDef (BSVInst _ bsv_def) = bsv_def
    in nub ([(getIdString def_id)] ++ (concatMap collectBSVDefNames (map getBSVDef children)))

-- #############################################################################
-- #
-- #############################################################################

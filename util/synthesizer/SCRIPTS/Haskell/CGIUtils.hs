module CGIUtils(
  -- HTML elements
  mkPage,        -- HTML a => a -> Html -> Html
  mkTable,       -- [[Html]] -> Html
  checkedBox,    -- String -> Html  (a preselected checkbox)

  -- Graphical hacks
  coloredRect,   -- String -> Int -> Int -> Html  i.e. color->width->height->Html 
  multiColoredRect, -- [(String,Int)]->Int->Html  i.e. [(color,width)]->heigth->Html
  mkOffsetBar,   -- Int -> Int -> Html   i.e. offset->relative->Html
  mkNormOffsetBar, -- Int -> Double -> Double -> Double -> Html
                   --width->maxBound-> offset ->relative->Html
  mkNormBar,     -- String -> Int -> Double -> Double -> Html (color->width->bound->value->Html)

  -- CGI helpers
  parseQueries,  -- String -> [(String,String)]
  unMime,        -- String -> String
  readHex,       -- String -> Int
 ) where

import Network.CGI
import Text.Html

import Utils(splitWhenT)
import Char(chr, ord)

--------------------------------------------------------------------------------
-- HTML elements

mkPage :: HTML a => a -> Html -> Html
mkPage title contents = header (thetitle (toHtml title)) +++ body contents


mkTable :: [[Html]] -> Html
mkTable rows = (table ! [intAttr "border" 0]) $ toHtml $ map mkRow rows
  where
  mkRow cols = (tr ! [strAttr "valign" "top"]) $ toHtml $ map mkCol cols
  mkCol :: Html -> Html
  mkCol = td

checkedBox :: String -> Html
checkedBox name = input ! [strAttr "type" "checkbox"
                          ,strAttr "name" name
                          ,emptyAttr "checked"]



--------------------------------------------------------------------------------
-- Graphical hacks

coloredRect :: String -> Int -> Int -> Html
coloredRect color width height = (table ! [intAttr "border" 0
                                          ,intAttr "width" width
                                          ,intAttr "cellpadding" 0
                                          ,intAttr "cellspacing" 0])
                                   (tr ((td![strAttr "bgcolor" color
                                            ,intAttr "height" height]) (toHtml "")))


multiColoredRect :: [(String,Int)] -> Int -> Html
multiColoredRect squares height = (table ! [intAttr "border" 0
                                           ,intAttr "cellpadding" 0
                                           ,intAttr "cellspacing" 0])
                                   (tr (concatHtml (map (\(color,width) -> td (coloredRect color width height)) squares)))


mkOffsetBar :: Int -> Int -> Html
mkOffsetBar offset relative
  | (-relative) > offset = mkOffsetBar offset (-offset)
  | relative < 0 = multiColoredRect [("#a0a0a0", offset+relative),("#00d000",-relative)] 14
  | otherwise    = multiColoredRect [("#a0a0a0", offset),("#d00000",relative)] 14

mkNormOffsetBar :: Int -> Double -> Double -> Double -> Html
mkNormOffsetBar width bound offset relative =
    mkOffsetBar (round (offset * factor))
                (round (relative * factor))
  where
  factor = fromIntegral width / bound

mkNormBar :: String -> Int -> Double -> Double -> Html
mkNormBar color width bound value = coloredRect color normalizedWidth 14
  where
  normalizedWidth = round (value*fromIntegral width/bound)

--------------------------------------------------------------------------------
-- CGI helpers
  
parseQueries :: String -> [(String,String)]
parseQueries [] = []
parseQueries str = let (q,qs) = splitWhenT ('&'==) str
                   in splitWhenT ('='==) q : parseQueries qs


unMime :: String -> String
unMime [] = []
unMime ('%':c1:c2:cs) = chr (readHex [c1,c2]) : unMime cs
unMime ('+':cs) = ' ':unMime cs
unMime (c:cs) = c:unMime cs
                       

readHex :: String -> Int
readHex = readHex' 0
  where
  readHex' acc [] = acc
  readHex' acc (d:ds) = readHex' (16*acc+hexDigit d) ds
  
  hexDigit d
    | '0' <= d && d <= '9' = ord d - ord '0'
    | 'a' <= d && d <= 'f' = ord d - ord 'a' + 10
    | 'A' <= d && d <= 'F' = ord d - ord 'A' + 10
    | otherwise = error ("readHex: \'" ++ d : "\' is not a hexadecimal digit")






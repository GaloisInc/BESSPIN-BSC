module NiceNamesHack(id_NiceNamesHack, nicefy) where

import Char(isSpace)

import Util(nubSort)

id_NiceNamesHack = "$Id$"

nicefy :: String -> String
nicefy str = concat [maybe t id (lookup t nices) | t <- toks]
    where toks = words' str
	  tmps = nubSort $ filter isTmpVar toks
	  nices = zip tmps niceIds

niceIds :: [String]
niceIds = allStrings ['a'..'z']

isTmpVar :: String -> Bool
isTmpVar ('_':_:_) = True
isTmpVar _ = False

allStrings :: [a] -> [[a]]
allStrings xs = concatMap (\n -> allStringsSize n xs) [1..]
    where allStringsSize 0 xs = [[]]
	  allStringsSize n xs = [x:ys | x <- xs, ys <- allStringsSize (n-1) xs]

words' :: String -> [String]
words' "" = []
words' s@(c:cs) | isTerminator c = [c] : words' cs
		| otherwise = w : words' s'
    where (w, s') = break isTerminator s

isTerminator t = t `elem` "([])," || isSpace t

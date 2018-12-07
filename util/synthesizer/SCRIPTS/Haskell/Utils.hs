module Utils(
  -- Pairs
                 -- first, second: apply function to one element of a pair
  first,         -- (a->b) -> (a,c) -> (b,c)
  second,        -- (b->c) -> (a,b) -> (a,c)

  -- Lists
  splitWhenT,    -- (a->Bool) -> [a] -> ([a],[a])
                 -- Splits a list in two at the first point where a
                 -- predicate holds. Throws away the split element.
  splitWhenAll,  -- (a->Bool) -> [a] -> [[a]]
                 -- Splits a list at all places where a predicate holds,
                 -- throwing away split elements.

  beginsWith,    -- Eq a => [a] -> [a] -> Bool
  endsWith,      -- Eq a => [a] -> [a] -> Bool

  transpose,     -- [[a]] -> [[a]]

  update,        -- Eq a => a -> b -> (b->b) -> [(a,b)] -> [(a,b)]  -- [(a,b)] is an association table

  -- Numbers
  showNumLen,    -- Num a => Int -> a -> String
  compareNum,    -- String -> String -> Ordering
                 -- Compares string containing numbers. E.g. "foo10" is considered to be
                 -- greater than "foo9", since 10 is greater than 9

  -- IO
  tryReadFile,   -- FilePath -> IO (Maybe String)
  tryReadFileStrict,
  tryMaybe,      -- IO a -> IO (Maybe a)

  -- combinators
  liftFun,       -- (a->b->c) -> (d->a) -> (d->b) -> (d->c)
                 -- liftFun lifts binary operators to function level.
                 -- Used to define the following:
  (&&&), (|||),  -- (a->Bool) -> (a->Bool) -> (a->Bool)                
 ) where

import IO
import Char(isDigit)

import ReadFileStrict

-----------------------------------------------------------------------------
-- Pairs

first :: (a->b) -> (a,c) -> (b,c)
first f (a,b) = (f a, b)

second :: (b->c) -> (a,b) -> (a,c)
second f (a,b) = (a, f b)

-----------------------------------------------------------------------------
-- Lists

-- splitWhenT split when a predicate is true, and throws away the element for which it is true
splitWhenT :: (a -> Bool) -> [a] -> ([a],[a])
splitWhenT p [] = ([],[])
splitWhenT p (x:xs)
  | p x = ([],xs)
  | otherwise = first (x:) (splitWhenT p xs)

splitWhenAll :: (a -> Bool) -> [a] -> [[a]]
splitWhenAll p [] = []
splitWhenAll p xs = let (as,bs) = splitWhenT p xs
                    in as : splitWhenAll p bs


beginsWith :: Eq a => [a] -> [a] -> Bool
beginsWith xs [] = True
beginsWith [] ys = False
beginsWith (x:xs) (y:ys) = x == y && beginsWith xs ys

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith xs ys = beginsWith (reverse xs) (reverse ys)


transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

update :: Eq a => a -> b -> (b->b) -> [(a,b)] -> [(a,b)]
update key new upd [] = [(key,new)]
update key new upd (kd@(k,d):kds)
  | key == k = (k, upd d):kds
  | otherwise = kd : update key new upd kds



-----------------------------------------------------------------------------
-- Showing numbers

-- doesn't work properly on negative numbers
showNumLen :: Num a => Int -> a -> String
showNumLen len a = let digits = show a
                    in replicate (len - length digits) '0' ++ digits

-- compare strings containing numbers. Sorts e.g. "10" > "9", since the number 10 is larger than 9, although not alphanumeric
compareNum :: String -> String -> Ordering
compareNum [] [] = EQ
compareNum [] ys = LT
compareNum xs [] = GT
compareNum (x:xs) (y:ys) 
  | isDigit x && isDigit y = let xNum = read (x:takeWhile isDigit xs) :: Integer
                                 yNum = read (y:takeWhile isDigit ys) :: Integer
                             in case compare xNum yNum of
                                   EQ -> compareNum (dropWhile isDigit xs) 
                                                    (dropWhile isDigit ys)
                                   o -> o
  | otherwise = case compare x y of
                   EQ -> compareNum xs ys
                   o -> o

-----------------------------------------------------------------------------
-- IO

tryMaybe :: IO a -> IO (Maybe a)
tryMaybe m = try m >>= \rslt -> case rslt of
                                  Left err -> return Nothing
                                  Right rslt' -> return (Just rslt')

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile fname = tryMaybe (readFile fname)

tryReadFileStrict :: FilePath -> IO (Maybe String)
tryReadFileStrict fname = tryMaybe (readFileStrict fname)


-----------------------------------------------------------------------------
-- Combinators

liftFun :: (a->b->c) -> (d->a) -> (d->b) -> (d->c)
liftFun op l r x = l x `op` r x

(&&&) = liftFun (&&)
(|||) = liftFun (||)



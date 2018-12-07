module OrdSet(
	OrdSet,
	empty, singleton, union, unionMany, add, addMany,
	intersect, intersectMany, delete, deleteMany, minus,
	map, fold, partition, -- filter, foldl, foldr,
	toList, fromList,
	null, 
	-- intersecting,
	isSubsetOf,
        size,
	elem
	-- replaceMaybe, substitute
	) where

import Prelude hiding(elem, length, null, map)
import qualified Data.Set as S
import ErrorUtil(internalError)


-- @@ Sets of ordered items.
-- using GHC library

newtype OrdSet a = OrdSet (S.Set a) deriving (Eq, Ord)

instance (Ord a, Show a) => Show (OrdSet a) where
    showsPrec p (OrdSet s) =
        showString "OrdSet:" . showsPrec p (S.toList s)
        
-- Empty table.
empty :: OrdSet a
empty = OrdSet S.empty

singleton :: a -> OrdSet a
singleton = OrdSet . S.singleton

map :: (Ord a, Ord b) => (a -> b) -> OrdSet a -> OrdSet b
map f (OrdSet s) = OrdSet (S.map f s)

fold :: (a -> b -> b) -> b -> OrdSet a -> b
fold f z (OrdSet s) = S.fold f z s 

partition :: (Ord a) => (a -> Bool) -> OrdSet a -> (OrdSet a, OrdSet a)
partition p (OrdSet s) = (OrdSet s1, OrdSet s2)
  where (s1, s2) = S.partition p s
  
union :: (Ord a) => OrdSet a -> OrdSet a -> OrdSet a
union (OrdSet s1) (OrdSet s2) = OrdSet (s1 `S.union` s2)

unionMany :: (Ord a) => [OrdSet a] -> OrdSet a
unionMany ordsets = OrdSet (S.unions [s | OrdSet s <- ordsets])

intersectMany :: (Ord a) => [OrdSet a] -> OrdSet a
intersectMany [] = internalError "OrdSet.intersectMany []"
intersectMany ordsets = OrdSet (foldr1 S.intersection [s | OrdSet s <- ordsets])

add :: (Ord a) => a -> OrdSet a -> OrdSet a
add elt (OrdSet set) = OrdSet (S.insert elt set)

addMany :: (Ord a) => [a] -> OrdSet a -> OrdSet a
addMany elts ordset = foldr add ordset elts

-- Look up an element.
elem :: (Ord a) => a -> OrdSet a -> Bool
elem elt (OrdSet set) = elt `S.member` set 

fromList :: (Ord a) => [a] -> OrdSet a
fromList = OrdSet . S.fromList

toList :: (Ord a) => OrdSet a -> [a]
toList (OrdSet set) = S.toList set

null :: OrdSet a -> Bool
null (OrdSet set) = S.null set

-- The following definitions are simply appauling
delete :: (Ord a) => a -> OrdSet a -> OrdSet a
delete elt (OrdSet set) = OrdSet (S.delete elt set)

deleteMany :: (Ord a) => [a] -> OrdSet a -> OrdSet a
deleteMany elts ordset = foldr delete ordset elts 

minus :: (Ord a) => OrdSet a -> OrdSet a -> OrdSet a
minus (OrdSet s1) (OrdSet s2) = OrdSet (s1 `S.difference` s2)

intersect :: (Ord a) => OrdSet a -> OrdSet a -> OrdSet a
intersect (OrdSet s1) (OrdSet s2) = OrdSet (s1 `S.intersection` s2)

isSubsetOf :: (Ord a) => OrdSet a -> OrdSet a -> Bool
isSubsetOf (OrdSet s) (OrdSet s') = s `S.isSubsetOf` s'

size (OrdSet s) = S.size s

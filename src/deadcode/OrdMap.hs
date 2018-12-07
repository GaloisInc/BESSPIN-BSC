module OrdMap(id_OrdMap,
	OrdMap(..),
	empty, singleton, add, add_C, (//),
	addMany, addMany_C,
        union, union_C,
        unionMany, unionMany_C,
	-- addKeep,
	delete, deleteMany,
	-- intersect, minus,
	-- partition, filter, foldl, foldr,
	toList, fromList, fromListWith,
	length,
	null, isSingleton,
	-- intersecting, subset
	elems, indices, indicesSet,
	--(!),
	lookup, lookupWithDefault --, lookupWithContinuation
	) where

-- Red-Black trees.
-- Implementation based on work by Norris Boyd, Andrew W. Appel,
-- David R. Tarditi, and Stephen J. Bevan.

import Prelude hiding (null, length, lookup)
import qualified Data.Set as S
import Eval
import ErrorUtil (internalError)
--import Util(traces)

id_OrdMap = "$Id$"

data Colour = Red | Black

data OrdMap a b
    = Empty
    | Node a b Colour (OrdMap a b) (OrdMap a b)

instance (Ord a, Show a, Show b) => Show (OrdMap a b) where
	show m = "(OrdMap " ++ show (toList m) ++ ")"
{-
	showsType x = showString "(OrdMap " . showsType (f x) . showString " " . showsType (g x) . showString ")"
		where f :: (Ord a) => OrdMap a b -> a
		      f _ = internalError "OrdMap.f"
		      g :: (Ord a) => OrdMap a b -> b
		      g _ = internalError "OrdMap.g"
-}

instance (Ord a, Eq b) => Eq (OrdMap a b) where
	x == y  =  toList x == toList y

rbiR :: a -> b -> OrdMap a b -> OrdMap a b -> OrdMap a b
rbiR k v (Node sk sv Red sl@(Node _ _ Red _ _) sr) (Node lk lv Red ll lr) =
    Node k v Red (Node lk lv Black ll lr) (Node sk sv Black sl sr)

rbiR k v (Node sk sv Red sl sr@(Node _ _ Red _ _)) (Node lk lv Red ll lr) =
    Node k v Red (Node lk lv Black ll lr) (Node sk sv Black sl sr)

rbiR k v (Node sk sv Red sl@(Node slk slv Red sll slr) sr) l =
    Node slk slv Black (Node k v Red l sll) (Node sk sv Red slr sr)

rbiR k v (Node sk sv Red sl sr@(Node _ _ Red _ _)) l =
    Node sk sv Black (Node k v Red l sl) sr

rbiR k v t l = Node k v Black l t

rbiL :: a -> b -> OrdMap a b -> OrdMap a b -> OrdMap a b
rbiL k v (Node lk lv Red ll lr@(Node _ _ Red _ _)) (Node rk rv Red rl rr) =
    Node k v Red (Node lk lv Black ll lr) (Node rk rv Black rl rr)

rbiL k v (Node lk lv Red ll@(Node _ _ Red _ _) lr) (Node rk rv Red rl rr) =
    Node k v Red (Node lk lv Black ll lr) (Node rk rv Black rl rr)

rbiL k v (Node lk lv Red ll lr@(Node lrk lrv Red lrl lrr)) r =
    Node lrk lrv Black (Node lk lv Red ll lrl) (Node k v Red lrr r)

rbiL k v (Node lk lv Red ll@(Node llk llv Red lll llr) lr) r =
    Node lk lv Black ll (Node k v Red lr r)

rbiL k v t r = Node k v Black t r

rbi :: (Ord a) => a -> b -> (b -> b) -> OrdMap a b -> OrdMap a b
rbi e v f Empty = Node e v Red Empty Empty
rbi e v f t@(Node k w Black l r) =
	case e `compare` k of
        LT -> rbiL k w (rbi e v f l) r
        EQ -> Node e (f w) Black l r
        GT -> rbiR k w (rbi e v f r) l

rbi e v f t@(Node k w Red l r) =
	case e `compare` k of
        LT -> Node k w Red (rbi e v f l) r
        EQ -> Node e (f w) Red l r
        GT -> Node k w Red l (rbi e v f r)

-- fixes up the tree after deletion
-- a is the node with an `extra' layer of black paint
rbdL = either rbdL' Right
-- case 0:
rbdL' (Node bk bv bc (Node ak av Red al ar) br) = Right $ Node bk bv bc (Node ak av Black al ar) br
-- case 1;
rbdL' b@(Node bk bv Black a d@(Node dk dv Red c e)) | black a && black c && black e = Right d'
    where b' = case rbdL' (Node bk bv Red a c) of
	       Right n -> n
	       Left (Node nk nv Red nl nr) -> Node nk nv Black nl nr
               _       -> (internalError "OrdMap: Unexpected Form")
	  d' = Node dk dv Black b' e
-- case 2
rbdL' b@(Node bk bv bc a d@(Node dk dv Black c e)) | black a && black c && black e = Left b'
    where d' = Node dk dv Red c e
	  b' = Node bk bv bc a d'
-- case 3
rbdL' b@(Node bk bv bc a d@(Node dk dv Black c@(Node ck cv Red cl cr) e)) | black a && black e = rbdL' b'
    where d' = Node dk dv Red cr e
	  c' = Node ck cv Black cl d'
	  b' = Node bk bv bc a c'
-- case 4
rbdL' b@(Node bk bv bc a d@(Node dk dv Black c e@(Node ek ev Red el er))) | black a = Right d'
    where e' = Node ek ev Black el er
	  b' = Node bk bv Black a c
	  d' = Node dk dv bc b' e'
rbdL' t = internalError $ "rbdL: RB invariant violated" -- ++ dumpTreeStruct 3 t

rbdR = either rbdR' Right
-- case 0
rbdR' (Node bk bv bc bl (Node ak av Red al ar)) = Right $ Node bk bv bc bl (Node ak av Black al ar)
-- case 1
rbdR' b@(Node bk bv Black d@(Node dk dv Red e c) a) | black a && black c && black e = Right d'
    where b' = case rbdR' (Node bk bv Red c a) of
	       Right n -> n
	       Left (Node nk nv Red nl nr) -> Node nk nv Black nl nr
               _       -> (internalError "OrdMap: Unexpected Form")
	  d' = Node dk dv Black e b'
-- case 2
rbdR' b@(Node bk bv bc d@(Node dk dv Black e c) a) | black a && black c && black e = Left b'
    where d' = Node dk dv Red e c
	  b' = Node bk bv bc d' a
-- case 3
rbdR' b@(Node bk bv bc d@(Node dk dv Black e c@(Node ck cv Red cl cr)) a) | black a && black e =  rbdR' b'
    where d' = Node dk dv Red e cl
	  c' = Node ck cv Black d' cr
	  b' = Node bk bv bc c' a
-- case 4
rbdR' b@(Node bk bv bc d@(Node dk dv Black e@(Node ek ev Red el er) c)	a) | black a = Right d'
    where e' = Node ek ev Black el er
	  b' = Node bk bv Black c a
	  d' = Node dk dv bc e' b'
rbdR' t = internalError $ "rbdR: RB invariant violated" -- ++ dumpTreeStruct 3 t

{-
dumpTreeStruct _ Empty = "()"
dumpTreeStruct 0 _ = "(...)"
dumpTreeStruct d (Node _ _ Black l r) = "\n" ++ take (40 - 4*d) (repeat ' ') ++ "(b " ++ dumpTreeStruct (d-1) l ++ " " ++
					dumpTreeStruct (d-1) r ++ ")"
dumpTreeStruct d (Node _ _ Red l r) = "\n" ++ take (40 - 4*d) (repeat ' ') ++ "(r " ++ dumpTreeStruct (d-1) l ++ " " ++
				      dumpTreeStruct (d-1) r ++ ")"

dumpTree d Empty = "\n" ++ take (40 - 4*d) (repeat ' ') ++ "X"
dumpTree 0 _ = take 40 (repeat ' ') ++ "<"
dumpTree d (Node k v Black l r) = dumpTree (d-1) r ++ "\n" ++ take (40 - 4*d) (repeat ' ') ++ "X:" ++ show k ++ dumpTree (d-1) l
dumpTree d (Node k v Red l r) = dumpTree (d-1) r ++  "\n" ++ take (40 - 4*d) (repeat ' ') ++ "O:" ++ show k ++  dumpTree (d-1) l
-}

rbd _ Empty = Right Empty -- root
rbd k n@(Node nk _ Red Empty Empty) =
    case compare k nk of
    EQ -> Right Empty
    _  -> Right n
rbd k n@(Node nk _ Black Empty Empty) =
    case compare k nk of
    EQ -> Left Empty -- extra black
    _  -> Right n -- not found
-- left child only
rbd k n@(Node nk nv Black l@(Node lk lv Red ll lr) Empty) =
    case compare k nk of
    EQ -> Right $ Node lk lv Black ll lr
    GT -> Right n -- not found
    LT -> rbdL $ lReplE n (rbd k l)
rbd k n@(Node nk nv Black l Empty) =
    case compare k nk of
    EQ -> Left l
    GT -> Right n -- not found
    LT -> rbdL $ lReplE n (rbd k l)
-- right child only
rbd k n@(Node nk nv Black Empty r@(Node rk rv Red rl rr)) =
    case compare k nk of
    EQ -> Right $ Node rk rv Black rl rr
    LT -> Right n -- not found
    GT -> rbdR $ rReplE n (rbd k r)
rbd k n@(Node nk _ Black Empty r) =
    case compare k nk of
    EQ -> Left $ r
    LT -> Right n -- not found
    GT -> rbdR $ rReplE n (rbd k r)
-- both children
rbd k n@(Node nk _ nc l r) =
    case compare k nk of
    LT -> rbdL $ lReplE n (rbd k l)
    GT -> rbdR $ rReplE n (rbd k r)
    EQ -> let (Node mk mv mc _ _) = minNode r
	  in  case (rbd mk r, mc) of
              (Right r', _)  ->  Right $ Node mk mv nc l r'
	      (Left r', _)   ->  rbdR $ Left (Node mk mv nc l r')

rReplE (Node k v c l _) (Left r) = Left (Node k v c l r)
rReplE (Node k v c l _) (Right r) = Right (Node k v c l r)
rReplE _ _ = internalError "rReplE"

lReplE (Node k v c _ r) (Left l) = Left (Node k v c l r)
lReplE (Node k v c _ r) (Right l) = Right (Node k v c l r)
lReplE _ _ = internalError "lReplE"

black :: OrdMap a b -> Bool
black Empty = True
black (Node _ _ Black _ _) = True
black _ = False

minNode Empty = internalError "OrdMap.min: empty tree"
minNode n@(Node _ _ _ Empty _) = n
minNode (Node _ _ _ l _) = minNode l


-- Empty table.
empty :: OrdMap a b
empty = Empty

singleton :: (Ord a) => (a, b) -> OrdMap a b
singleton (k, v) = Node k v Black Empty Empty

null :: OrdMap a b -> Bool
null Empty = True
null _ = False

length :: OrdMap a b -> Int
length Empty = 0
length (Node _ _ _ l r) = 1 + length l + length r

isSingleton :: OrdMap a b -> Bool
isSingleton (Node _ _ _ Empty Empty) = True
isSingleton _ = False

elems :: OrdMap a b -> [b]
elems Empty = []
elems (Node k v _ l r) = elems l ++ v : elems r

indices :: OrdMap a b -> [a]
indices Empty = []
indices (Node k v _ l r) = indices l ++ k : indices r

indicesSet :: (Ord a) => OrdMap a b -> S.Set a
indicesSet Empty = S.empty
indicesSet (Node k v _ l r) =
    S.unions [indicesSet l, S.singleton k, indicesSet r]

-- Insert an element overwriting an existing one with the same key.
add :: (Ord a) => (a,  b) -> OrdMap a b -> OrdMap a b
add p t = add_C const p t

add_C :: (Ord i) => (a->a->a) -> (i, a) -> OrdMap i a -> OrdMap i a
add_C comb (e, v) t =
    case rbi e v (comb v) t of
        Node k v Red l@(Node _ _ Red _ _) r -> Node k v Black l r
        Node k v Red l r@(Node _ _ Red _ _) -> Node k v Black l r
        x                                   -> x

addMany :: (Ord a) => [(a, b)] -> OrdMap a b -> OrdMap a b
addMany is s = foldr add s is

addMany_C :: (Ord a) => (b->b->b) -> [(a, b)] -> OrdMap a b -> OrdMap a b
addMany_C comb is s = foldr (add_C comb) s is

union :: (Ord a) => OrdMap a b -> OrdMap a b -> OrdMap a b
union t1 t2 = addMany (toList t2) t1

union_C :: (Ord a) => (b->b->b) -> OrdMap a b -> OrdMap a b -> OrdMap a b
union_C comb t1 t2 = addMany_C comb (toList t2) t1

unionMany :: (Ord a) => [OrdMap a b] -> OrdMap a b
unionMany = foldr union empty

unionMany_C :: (Ord a) => (b->b->b) -> [OrdMap a b] -> OrdMap a b
unionMany_C comb = foldr (union_C comb) empty

(//) :: (Ord a) => OrdMap a b -> [(a, b)] -> OrdMap a b
t // [] = t
t // (xy:xys) = add xy t // xys


delete :: (Ord a) => a -> OrdMap a b -> OrdMap a b
delete k n = case rbd k n of
	     Right n' -> n'
	     Left (Node nk' nv' _ nl' nr') -> Node nk' nv' Black nl' nr'
	     Left Empty -> Empty

deleteMany :: (Ord a) => [a] -> OrdMap a b -> OrdMap a b
deleteMany ks m = foldl (flip delete) m ks

lookup :: (Ord a) => a -> OrdMap a b -> Maybe b
lookup _ Empty = Nothing
lookup e (Node k v _ l r) =
    case e `compare` k of
      LT -> lookup e l
      EQ -> Just v
      GT -> lookup e r

-- Map a function over the values.
instance (Ord a) => Functor (OrdMap a) where
    --map :: (b->c) -> OrdMap a b -> OrdMap a c
    fmap f Empty = Empty
    fmap f (Node k v c l r) = Node k (f v) c (fmap f l) (fmap f r)

lookupWithDefault :: (Ord a) => OrdMap a b -> b -> a -> b
lookupWithDefault Empty d _ = d
lookupWithDefault (Node k v _ l r) d e =
	case e `compare` k of
        LT -> lookupWithDefault l d e
        EQ -> v
        GT -> lookupWithDefault r d e

fromList :: (Ord a) => [(a,b)] -> OrdMap a b
fromList is = addMany is empty

fromListWith :: (Ord a) => (b -> b -> b) -> [(a,b)] -> OrdMap a b
fromListWith comb is = addMany_C comb is empty

--for efficiency, toList is now implemented with an accumulating parameter
toList :: OrdMap a b -> [(a, b)]
toList = toList' []
  where
  toList' acc Empty = acc
  toList' acc (Node k v _ l r) = toList' ((k,v):toList' acc r) l

instance (Ord a, Eq b) => Hyper (OrdMap a b) where
    hyper m y = (m == m) `seq` y

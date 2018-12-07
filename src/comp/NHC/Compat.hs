module Compat(id_Compat, tdPicosec, unionBy) where
import Time
import List(deleteBy, nubBy)

id_Compat = " $Id: Compat.hs,v 1.3 2001/11/02 12:17:33 augustss Exp $ "

-- tdPicosec = tdPicoSec

unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys         = xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs


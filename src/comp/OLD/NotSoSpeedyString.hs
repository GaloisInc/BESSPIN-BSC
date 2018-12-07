module NotSoSpeedyString(id_NotSoSpeedyString,
			 SString, toString, fromString, (++), concat) where

import Prelude hiding((++), concat)
import qualified Prelude((++))
import Hash(Hashable(..))

id_NotSoSpeedyString = "$Id$"

data SString = SString String deriving (Eq, Ord)

instance Show SString where
    show (SString s) = show s

instance Hashable SString where
    hash (SString s) = hash s

toString :: SString -> String
toString (SString s) = s

fromString :: String -> SString
fromString = SString

(++) :: SString -> SString -> SString
s ++ s' = fromString $ (Prelude.++) (toString s)  (toString s')

concat :: [SString] -> SString
concat = fromString . concatMap toString

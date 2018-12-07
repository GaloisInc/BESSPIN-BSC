module TmpNam(tmpNam) where
import Random

tmpNam :: IO String
tmpNam = do
         x <- randomIO :: IO Int
         return $ "/tmp/bsc" ++ show x

module TmpNam(tmpNam) where
import GetPid(getPid)

tmpNam :: IO String
tmpNam = do
         x <- getPid
         return $ "/tmp/bsc" ++ show x

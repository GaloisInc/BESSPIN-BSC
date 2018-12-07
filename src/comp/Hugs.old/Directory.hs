module Directory(getModificationTime, removeFile) where
import Time

getModificationTime :: String -> IO ClockTime
getModificationTime s = error "Hugs does not implement Haskell correctly, getModificationTime is missing"

removeFile :: String -> IO ()
removeFile s = error "Hugs does not implement Haskell correctly, removeFile is missing"

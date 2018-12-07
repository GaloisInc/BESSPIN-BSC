module ReadFileStrict (readFileStrict) where

import System.IO
import System.IO.Unsafe
import Foreign
import Data.Char

readFileStrict f = do
  h <- openBinaryFile f ReadMode
  s <- hFileSize h
  fp <- mallocForeignPtrBytes (fromIntegral s)
  len <- withForeignPtr fp $ \buf -> hGetBuf h buf (fromIntegral s)
  lazySlurp fp 0 len

buf_size = 4096 :: Int

lazySlurp :: ForeignPtr Word8 -> Int -> Int -> IO String
lazySlurp fp ix len
  | fp `seq` False = undefined
  | ix >= len = return []
  | otherwise = do
      cs <- unsafeInterleaveIO (lazySlurp fp (ix + buf_size) len)
      ws <- withForeignPtr fp $ \p -> loop (min (len-ix) buf_size - 1) 
					((p :: Ptr Word8) `plusPtr` ix) cs
      return ws
 where
  loop :: Int -> Ptr Word8 -> String -> IO String
  loop len p acc
    | len `seq` p `seq` False = undefined
    | len < 0 = return acc
    | otherwise = do
       w <- peekElemOff p len
       loop (len-1) p (chr (fromIntegral w):acc)


module BinaryIO(writeBinaryFile, readBinaryFile) where
import IO

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile name str = writeFile name str
readBinaryFile :: FilePath -> IO String
readBinaryFile name = readFile name

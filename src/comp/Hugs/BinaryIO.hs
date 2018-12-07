module BinaryIO(BinaryIO.writeBinaryFile, BinaryIO.readBinaryFile) where
import IO
import IOExts

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile name str = writeFile name str
readBinaryFile :: FilePath -> IO String
readBinaryFile name = readFile name

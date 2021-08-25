-- bytestringcopy
import Control.Exception
import System.IO
import System.Directory
import System.Environment
import qualified Data.ByteString.Lazy as B


main :: IO ()
main = do
    (fileName1 : fileName2 : _) <- getArgs
    copy fileName1 fileName2


copy :: FilePath -> FilePath -> IO ()
copy source dest = do
    contents <- B.readFile source
    bracketOnError (openTempFile "./io-files/" "temp")
                   (\(tempName, tempHandle) -> do
                      hClose tempHandle
                      removeFile tempName)
                   (\(tempName, tempHandle) -> do
                      B.hPutStr tempHandle contents
                      hClose tempHandle
                      renameFile tempName dest)

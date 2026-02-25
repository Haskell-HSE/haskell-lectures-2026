import Control.Concurrent
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy as L
import System.Environment (getArgs)

main :: IO ()
main = do
    [fileA, fileB] <- getArgs
    _ <- forkIO ({- lock? -} hashAndPrint fileA {- unlock? -})
    hashAndPrint fileB
    -- try to acquire lock

hashAndPrint :: FilePath -> IO ()
hashAndPrint f = do
    file <- L.readFile f
    let digest = md5 file
    putStrLn (f ++ ": " ++ show digest)

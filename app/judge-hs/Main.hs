module Main where

import qualified Data.ByteString.Char8 as B
import Data.Bool
import Data.List
import Data.Time
import System.Environment
import System.Directory
import System.FilePath
import System.Process

main :: IO ()
main = do
    probid:projid:_ <- getArgs
    fs <- sort . filter (isExtensionOf "txt") 
            <$> listDirectory ("test" </> "case" </> probid </> "in")
    let ins  = map (("test" </>) . ("case" </>) . (probid </>) . ("in" </>)) fs
    let outs = map (("test" </>) . ("case" </>) . (probid </>) . ("out" </>)) fs
    loop projid ins outs

loop :: String -> [FilePath] -> [FilePath] -> IO ()
loop _   []     _      = return ()
loop cmd (i:is) (o:os) = do
    start <- getCurrentTime
    (_, Just hout, _, _)
            <- createProcess 
                (shell ("stack exec " ++ cmd 
                 -- ++ " +RTS -p -RTS"
                 ++ " < " ++ i))
                { std_out = CreatePipe }
    out0 <- B.hGetContents hout
    out1 <- B.filter (/= '\r') <$> B.readFile o
    let msg = bool "WA" "AC" (out0 == out1)
    putStr (takeFileName i ++ ": " ++ msg ++ " : ")
    end <- getCurrentTime
    print (diffUTCTime end start)
    loop cmd is os
loop _ _ _ = error "loop: impossible"

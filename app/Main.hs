module Main where

import Computer
import System.IO

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    boot
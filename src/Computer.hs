module Computer where

import FileManager
import UserManager
import Command
import Status
import Data.List
import Control.Monad

boot :: IO ()
boot = terminal Status {currDir=[], fileStructure=example, currUser="Matthiti_13"}

terminal :: Status -> IO ()
terminal s = do
  putStr $ (currUser s) ++ ":" ++ (pathToString (currDir s)) ++ " $ "
  input <- getLine
  if null input
    then terminal s
    else if (command (parseCommand input)) == "exit"
      then return ()
      else do
        newStatus <- executeCommand s (parseCommand input)
        terminal newStatus

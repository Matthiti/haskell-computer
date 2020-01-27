module Command where

import FileManager
import Status
import Data.List
import Data.Maybe

data Command = Command { command :: String
                       , args :: Args
                       }
                      
type Args = [String]

parseCommand :: String -> Command
parseCommand s = Command{command=head (words s), args=tail (words s)}

executeCommand :: Status -> Command -> IO (Status)
-- executeCommand s (Command "ls" _) = do
--   when (isJust files) $ putStrLn $ intercalate "\t" $ map show $ fromJust files
--   return s
--   where files = getDirectoryContents (fileStructure s) (currDir s)
executeCommand s c@(Command "ls" args) = ls s args
executeCommand s c = do
  putStrLn $ "Command not found: " ++ command c
  return s

ls :: Status -> Args -> IO (Status)
ls s [] = ls s $ [pathToString $ currDir s]
ls s (x:[]) 
  | isJust files = do
      showResult $ files
      return s
  | otherwise = do
      putStrLn $ "Unknown directory: " ++ x
      return s
  where files = getDirectoryContents (fileStructure s) (stringToPath x)
        showResult f = putStrLn $ intercalate "\t" $ map show $ fromJust f
ls s xs = do
  mapM (\x -> ls s [x]) xs
  return s
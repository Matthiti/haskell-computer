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
executeCommand s (Command "ls" args) = ls s args
executeCommand s (Command "cd" args) = cd s args
executeCommand s (Command "mkdir" args) = mkdir s args
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

-- |if lenght(args) > 1, only the first one will be used
cd :: Status -> Args -> IO (Status)
cd s [] = return s { currDir = []}
cd s (x:_)
  | isJust result  = return s { currDir = path }
  | otherwise = do
    putStrLn $ "Unknown directory: " ++ x
    return s
  where result = goToDirectory (fileStructure s) path
        path   = absolutePath (fileStructure s) (currDir s) (stringToPath x)

mkdir :: Status -> Args -> IO (Status)
mkdir s [] = do
  putStrLn "No directory specified"
  return s
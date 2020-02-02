module FileManager
  (
    File(name),
    FileStructure,
    Path,
    pathToString,
    stringToPath,
    absolutePath,
    base,
    getDirectoryContents,
    goToDirectory,
    example
  ) where

import Util
import Data.List
import Data.Maybe

data Rights = Rights { read :: Bool
                     , write :: Bool
                     , execute :: Bool
                     } deriving (Show)

data File = File { identifier :: Int
                 , name :: String
                 , size :: Int
                 , rights :: Rights
                 }
          | Folder { identifier :: Int
                   , name :: String
                   , rights :: Rights
                   }
  
instance Show File where
  show (File _ name _ _) = name
  show (Folder _ name _) = name ++ "/"

data FileStructure = FolderStructure File [FileStructure]
                   | FileStructure File

-- instance Show FileStructure where
--   show (FolderStructure f xs) = name f ++ show xs
--   show (FileStructure x) = name x

type Path = [String]

pathToString :: Path -> String
pathToString p = "/" ++ intercalate "/" p

stringToPath :: String -> Path
stringToPath s = split '/' s

absolutePath :: FileStructure -> Path -> Path -> Path
absolutePath fs currPath [] = currPath
absolutePath fs currPath ("..":xs)
  | null currPath = absolutePath fs currPath xs
  | otherwise     = absolutePath fs (init currPath) xs
absolutePath fs currPath (".":xs) = absolutePath fs currPath xs
absolutePath fs currPath (x:xs) = absolutePath fs (currPath ++ [x]) xs

pathSeperator :: String
pathSeperator = "/"

base :: FileStructure
base = FolderStructure Folder {identifier=0, name="/", rights=Rights{FileManager.read=True, write=False, execute=False}} []

example :: FileStructure
example = FolderStructure Folder {identifier=1, name="/", rights=Rights{FileManager.read=True, write=False, execute=False}}
  [
    FileStructure File {identifier=2, name="test.js", size=4, rights=Rights{FileManager.read=True, write=True, execute=True}},
    FolderStructure Folder {identifier=3, name="home", rights=Rights{FileManager.read=True, write=False, execute=False}}
    [
      FileStructure File {identifier=2, name="test.html", size=4, rights=Rights{FileManager.read=True, write=True, execute=True}}
    ]
  ]

getDirectoryContents :: FileStructure -> Path -> Maybe [File]
getDirectoryContents fs@(FolderStructure _ xs) path = do
  folder <- goToDirectory fs path
  return $ map root (contents folder)
  where contents (FolderStructure _ xs) = xs

goToDirectory :: FileStructure -> Path -> Maybe FileStructure
goToDirectory (FileStructure _) _ = Nothing
goToDirectory fs [] = Just fs
goToDirectory (FolderStructure f xs) (p:ps) 
  | isJust nextFolder = goToDirectory (fromJust nextFolder) ps
  | otherwise = Nothing
  where folders = filter isFolder xs
        nextFolder = find (\x -> (name (root x)) == p) folders
  
createDirectory :: FileStructure -> Path -> Maybe FileStructure
createDirectory (FileStructure _) _ = Nothing
createDirectory fs [] = Just fs
createDirectory fs@(FolderStructure f xs) (p:ps)
  | isJust next = do
      newStructure <- createDirectory (fromJust next) ps
      return $ FolderStructure f (newStructure:rest)
  | otherwise = Just fs -- TODO: Implement
  where next = find (\x -> name (root x) == p) xs
        rest = filter (\x -> name (root x) /= p) xs

exists :: FileStructure -> Path -> Bool
exists fs [] = True
exists (FileStructure f) (p:[]) = name f == p
exists (FileStructure f) _ = False
exists (FolderStructure _ xs) (p:ps)
  | isJust next = exists (fromJust next) ps
  | otherwise   = False
  where next = find (\x -> name (root x) == p) xs
      
isFolder :: FileStructure -> Bool
isFolder (FolderStructure _ _) = True
isFolder _ = False

root :: FileStructure -> File
root (FolderStructure f _) = f
root (FileStructure f)     = f
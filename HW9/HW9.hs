import Data.Char
import System.FilePath
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import Prelude hiding (catch)

quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = (quickSort lessers) ++ [x] ++ (quickSort greaters)
    where lessers  = [ n | n <- xs, n <= x ]
          greaters = [ n | n <- xs, n > x ]

enemies :: IO ()
enemies = do
    dfe <- doesFileExist "enemies.txt"
    putStrLn "Would you like Remove or Add enemies, or Exit?"
    input <- getLine

    if (map toLower input) == "add" then do

        putStrLn "Add enemy name: "
        add <- getLine

        if (dfe) then do
            
            addEnemy add
            enemies

        else do

            writeFile "enemies.txt" ""
            addEnemy add
            enemies

    else do 

        if (map toLower input) == "remove" then do 

            putStrLn "Input enemy name: "
            remove <- getLine
            removeEnemy remove
            enemies

        else do

            if (map toLower input) == "exit" then do 

                putStrLn "Bye Bye"
                return ()

            else do

                putStrLn "I don't understand"
                enemies

addEnemy :: String -> IO ()
addEnemy name = do
    dfe <- doesFileExist "enemies.txt"
    if (dfe) then do
        contents <- readFile "enemies.txt"
        let newcontents = unlines $ quickSort (name:(lines contents))
    
        writeFile "temp.txt" newcontents
        renameFile "temp.txt" "enemies.txt"
    else putStrLn "Can't find the enemies!"

removeEnemy :: String -> IO ()
removeEnemy name = do
    contents <- readFile "enemies.txt"
    let newcontents = unlines $ delete name (lines contents)
    
    writeFile "temp.txt" newcontents
    renameFile "temp.txt" "enemies.txt"

counter :: Char -> FilePath -> IO ()
counter c file = do
    s <- readFile file
    putStrLn (show(length $ filter (== c) s))
     
bigExtension :: FilePath -> IO ()
bigExtension file =  renameExt file `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = putStrLn "does not exist!"
          | otherwise = throwIO e
            
renameExt :: FilePath -> IO ()
renameExt file = do
    let path = splitExtension file
    let ext = (map toUpper (concat path))
    let newPath = replaceExtension file ext
    renamePath file  newPath

copiesFile :: FilePath -> FilePath -> IO ()
copier old new =  (copiesFile old new) `catch` handleExists
 where handleExists e
          | isDoesNotExistError e = putStrLn "original does not exist!"
          | otherwise = throwIO e
 
copiesFile :: FilePath -> FilePath -> IO ()
copiesFile original new = do
      s <- readFile original
      writeFile new s
      



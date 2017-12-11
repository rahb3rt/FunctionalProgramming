import Project3package
import System.Random
import System.Process
import System.Directory
import Data.Char

randomL :: [a] -> IO a
randomL l = do
    seed <- newStdGen
    let i = fst (randomR (0,length l-1) seed)
    return $ l!!i

randomImage x y name = do

    let pixels = []
    let i = 0

    image <- createImage x y i pixels

    writeJPG name image


createImage x y i pixels = do 

    if i < (x * y) then do 

        pixel  <- createPixel
        let insertPixels = take 1 (repeat pixel)
        let tmpPixels  = pixels
        let pixels = tmpPixels ++ insertPixels
        let j = i
        let i = j + 1
        createImage x y i pixels

    else do

        return (Im x y pixels)


createPixel = do

    r <- randomL [0..255]
    g <- randomL [0..255]
    b <- randomL [0..255]

    let pixel = RGB r g b

    return pixel

fstP (RGB a _ _) = a
sndP (RGB _ a _) = a
trdP (RGB _ _ a) = a

fstI (Im a _ _) = a
sndI (Im _ a _) = a
trdI (Im _ _ a) = a

darken delta path = do

    image <- readJPG path
    let dPixels = []

    let pixels = trdI image
    picture <- darkenP delta pixels dPixels 0 (fstI image) (sndI image)

    writeJPG path picture

darkenP delta pixels dPixels i x y = do

    if i < length pixels then do
        
        let r = deltaCheck delta (fstP (pixels !! i))
        let g = deltaCheck delta (sndP (pixels !! i))
        let b = deltaCheck delta (trdP (pixels !! i))
        let pixel = RGB r g b
        let tmpPixels = dPixels
        let dPixels = tmpPixels ++ (take 1 (repeat pixel))
        let j = i
        let i = j + 1
        darkenP delta pixels dPixels i x y
    

    else do

        return (Im x y dPixels)



deltaCheck :: Int-> Int -> Int
deltaCheck delta channel = 
  if (channel + delta) > 255 then 255
  
  else 
      if (channel + delta) < 0 then 0 
          
      else channel + delta
          
colorSwap path = do

    image <- readJPG path
    let swapPixels = []

    let pixels = trdI image
    picture <- swap pixels swapPixels 0 (fstI image) (sndI image)

    writeJPG path picture


swap :: [Pixel] -> [Pixel] -> Int -> Int -> Int -> IO Image
swap pixels swapPixels i x y = do

    if i < length pixels then do
        
        let r = fstP (pixels !! i)
        let g = sndP (pixels !! i)
        let b = trdP (pixels !! i)
        pixel <- swapped [r, g, b]
        let pixelsSwap = (RGB (pixel !! 0) (pixel !! 1) (pixel !! 2)) 
        let tmpPixels = swapPixels
        let swapPixels = tmpPixels ++ (take 1 (repeat pixelsSwap))
        let j = i
        let i = j + 1
        swap pixels swapPixels i x y
    

    else do

        return (Im x y swapPixels)

swapped x = if length x < 2 then return x else do
    i <- randomRIO (0, length(x)-1)
    r <- swapped (take i x ++ drop (i+1) x)
    return (x!!i : r)

noisy path = do

    image <- readJPG path
    let swapPixels = []

    let pixels = trdI image
    picture <- swapNoisy pixels swapPixels 0 (fstI image) (sndI image)

    writeJPG path picture


swapNoisy :: [Pixel] -> [Pixel] -> Int -> Int -> Int -> IO Image
swapNoisy pixels swapPixels i x y = do

    if i < length pixels then do
        
        let r = fstP (pixels !! i)
        let g = sndP (pixels !! i)
        let b = trdP (pixels !! i)

        chance <- randomL [0..9]

        if chance == 0 then do
        
            pixel <-  swapped [r, g, b]
            let pixelsSwap = (RGB (pixel !! 0) (pixel !! 1) (pixel !! 2)) 
            let tmpPixels = swapPixels
            let swapPixels = tmpPixels ++ (take 1 (repeat pixelsSwap))
            let j = i
            let i = j + 1
            swap pixels swapPixels i x y
 

        else do

            let pixel = [r, g, b]
            let pixelsSwap = (RGB (pixel !! 0) (pixel !! 1) (pixel !! 2)) 
            let tmpPixels = swapPixels
            let swapPixels = tmpPixels ++ (take 1 (repeat pixelsSwap))
            let j = i
            let i = j + 1
            swap pixels swapPixels i x y
           
    else do

        return (Im x y swapPixels)

view image = do

    let i = 0
    path <- fileName i

    writeJPG path image
    callCommand ("open " ++ path)
  
fileName i = do

    paths <- getDirectoryContents "."
    if ((".~" ++ show(i) ++ ".jpg")) `elem` paths then do

        let j = i
        let i = j + 1
        fileName i

    else do 
    
        return (".~" ++ show(i) ++ ".jpg")

cleanup = do

    let i = 0
    output <- clean i
    putStrLn output

clean i = do

    paths <- getDirectoryContents "."
    if ((".~" ++ show(i) ++ ".jpg")) `elem` paths then do

        callCommand ("rm .~*.jpg")
        let j = i
        let i = j + 1
        clean i

    else do 

        return "Ok I did it"

pictureTime = do

    putStrLn "Enter filepath to image"
    path <- getLine

    dfe <- doesFileExist path

    if dfe then do

       picture path

    else do

        putStrLn "The file does not exist, try again"
        pictureTime

picture path = do

    putStrLn "Choose an option from the list below:"
    putStrLn "View, Image"
    putStrLn "Open, New Image"
    putStrLn "Darken, Make Image Darker"
    putStrLn "Lighten, Make Image Lighter"
    putStrLn "Swap, Swap Colors in Image"
    putStrLn "Noisy, Make Image Noisy"
    putStrLn "Rotate, Rotate Image 90 degrees"
    putStrLn "Random, Create Random Image"
    putStrLn "Save, Image to New File"
    putStrLn "Quit"
    input <- getLine
        
    if (map toLower input) == "view" then do

        putStrLn path
        viewImage <- readJPG path
        view viewImage
        picture path

    else do

        if (map toLower input) == "open" then do

            putStrLn "Open a new image: "
            tmpPath <- getLine 

            dfe <- doesFileExist tmpPath

            if dfe then do

                let path = tmpPath
                putStrLn "Okay I did it"
                picture path

            else do 

                putStr "File does not Exist, renter command"
                picture path

        else do
    
            if (map toLower input) == "save" then do

                putStrLn "What would you like to name the file"
                savePath <- getLine
                image <- readJPG path
                let path = (savePath ++ ".jpg")
                writeJPG path image
                cleanup
                picture path

            else do
    
                if (map toLower input) == "quit" then do

                    cleanup
                    putStrLn "Bye Bye"
                    return()

                else do 

                    if (map toLower input) == "swap" then do

                        colorSwap path
                        putStrLn "Ok all set"
                        picture path
 
                    else do

                        if (map toLower input) == "darken" then do
                                        
                            putStrLn "By how much would you like to darken the image?"
                            input <- getLine
                            let delta = (-1) * (read input :: Int)
                            darken delta path
                            putStrLn "Ok all set"
                            picture path
 
                        else do

                            if (map toLower input) == "lighten" then do
                                
                                putStrLn "By how much would you like to lighten the image?"
                                input <- getLine
                                let delta = abs (read input :: Int)
                                darken delta path
                                putStrLn "Ok all set"
                                picture path
 
                            else do

                                if (map toLower input) == "noisy" then do
                                    
                                    noisy path
                                    putStrLn "Ok all set"
                                    picture path
 
                                else do

                                    if (map toLower input) == "rotate" then do

                                        putStrLn "ok all set"
                                        picture path
                                    
                                    else do 

                                        if (map toLower input) == "random" then do
                                            
                                            putStrLn "Please input name for random image:"
                                            name <- getLine
                                            let path = name ++ ".jpg"
                                            putStrLn "Please input the height: "
                                            y<- getLine
                                            putStrLn "Please input the width:"
                                            x<- getLine
                                            randomImage (read x::Int) (read y::Int) path 
                                            putStrLn "Ok all set"
                                            picture path
 
                                        else do

                                            putStrLn "I don't understand"
                                            picture path








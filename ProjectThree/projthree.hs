{--Robert Davis--}

import Project3package
import System.Random
import System.Process
import System.Directory
import Data.Char
import Data.List.Split
import Data.List

fir :: Image -> Int
fir (Im a _ _) = a

scd :: Image -> Int
scd (Im _ a _) = a

trd :: Image -> [Pixel]
trd (Im _ _ a) = a

getFirst :: Image -> Int
getFirst (Im x _ _) = x

getSecond :: Image -> Int
getSecond (Im _ x _) = x

getEnd :: [a] -> [a] 
getEnd x = reverse (tail (reverse x))

border :: Pixel -> Image -> Image
border x y = 
    (colStackLast  (take (getSecond y + 2) (repeat (x)))
    (colStackFirst (take (getSecond y + 2) (repeat (x))) 
    (rowStackLast  (take (getFirst y) (repeat (x))) 
    (rowStackFirst (take (getFirst y) (repeat (x))) y))))

thickBorder :: (Eq t, Num t) => Image -> Pixel -> t -> Image
thickBorder x y 0     = x
thickBorder x y times = thickBorder (border (y) x) y  (times - 1)

rotateCW :: [[a]] -> [[a]]
rotateCW = map reverse . transpose 
rotate image = (Im (scd image) (fir image) (concat(rotateCW (blocks image))))

blocks :: Image -> [[Pixel]]
blocks (Im x _ y) = (chunksOf x y)
   
dropFirstCol :: Image -> Image
dropFirstCol x = (Im (getFirst x-1) (getSecond x)
    (concat (map (tail) (blocks x))))
      
dropLastCol :: Image -> Image
dropLastCol x = (Im (getFirst x -1) (getSecond x)
    (concat (map (getEnd) (blocks x))))
         
colStackFirst :: [Pixel] -> Image -> Image 
colStackFirst x y = (Im (getFirst y + 1)  
    (getSecond y) (concat (zipWith (:) x (blocks y))))    
            
colStackLast :: [Pixel] -> Image -> Image 
colStackLast x y = (Im (getFirst y + 1) (getSecond y)
    (concat (map (reverse) (zipWith (:) x (map (reverse) (blocks y))))))

rowStackFirst :: [Pixel] -> Image -> Image
rowStackFirst stack image = (Im (fir image) (scd image + 1) (stack ++ trd image))
   
rowStackLast :: [Pixel] -> Image -> Image
rowStackLast stack image = (Im (fir image) (scd image + 1) (trd image ++ stack))


randomL :: [a] -> IO a
randomL l = do
    seed <- newStdGen
    let i = fst (randomR (0,length l-1) seed)
    return $ l!!i

randomImage :: Int -> Int -> FilePath -> IO ()
randomImage x y name = do

    let pixels = []
    let i = 0

    image <- createImage x y i pixels

    writeJPG name image

createImage :: Int -> Int -> Int -> [Pixel] -> IO Image
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

createPixel :: IO Pixel
createPixel = do

    r <- randomL [0..255]
    g <- randomL [0..255]
    b <- randomL [0..255]

    let pixel = RGB r g b

    return pixel

fstP :: Pixel -> Int
fstP (RGB a _ _) = a

sndP :: Pixel -> Int
sndP (RGB _ a _) = a

trdP :: Pixel -> Int
trdP (RGB _ _ a) = a

fstI :: Image -> Int
fstI (Im a _ _) = a

sndI :: Image -> Int
sndI (Im _ a _) = a

trdI :: Image -> [Pixel]
trdI (Im _ _ a) = a

darken :: Int -> FilePath -> IO () 
darken delta path = do

    image <- readJPG path
    let dPixels = []

    let pixels = trdI image
    picture <- darkenP delta pixels dPixels 0 (fstI image) (sndI image)

    writeJPG path picture

darkenP :: Monad m => Int -> [Pixel] -> [Pixel] -> Int -> Int -> Int -> m Image
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
          
colorSwap :: FilePath -> IO ()
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

swapped :: [a] -> IO [a]
swapped x = if length x < 2 then return x else do
    i <- randomRIO (0, length(x)-1)
    r <- swapped (take i x ++ drop (i+1) x)
    return (x!!i : r)

strips :: Int -> FilePath -> IO [Char] 
strips n path = do

    image <- readJPG path
    let pixels = trdI image
    let tmpPixels = chunksOf (fstI image) pixels
    let pixels = chunksOf ((sndI image) `div` n) tmpPixels
    pixels <- swapped (pixels)
    let tmpPixels = concat (map concat pixels)
    let tmpImage = Im (fstI image) (sndI image) tmpPixels
    output <- fileName 0

    writeJPG output tmpImage

    return output
    

noisy :: FilePath -> IO ()
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

view :: Image -> IO ()
view image = do

    let i = 0
    path <- fileName i

    writeJPG path image
    callCommand ("open " ++ path)

fileName :: (Show a, Num a) => a -> IO [Char]
fileName i = do

    paths <- getDirectoryContents "."
    if ((".~" ++ show(i) ++ ".jpg")) `elem` paths then do

        let j = i
        let i = j + 1
        fileName i

    else do 
    
        return (".~" ++ show(i) ++ ".jpg")

cleanup :: IO ()
cleanup = do

    let i = 0
    output <- clean i
    putStrLn output

clean :: (Show t, Num t) => t -> IO [Char]
clean i = do

    paths <- getDirectoryContents "."
    if ((".~" ++ show(i) ++ ".jpg")) `elem` paths then do

        callCommand ("rm .~*.jpg")
        let j = i
        let i = j + 1
        clean i

    else do 

        return "Ok I did it"

pictureTime :: IO ()
pictureTime = do

    putStrLn "Enter name to image"
    name <- getLine

    let path = name ++ ".jpg"

    dfe <- doesFileExist path

    if dfe then do

       picture path

    else do

        putStrLn "The file does not exist, try again"
        pictureTime

picture :: String -> IO ()
picture path = do

    putStrLn "Your current image is:\n"
    putStrLn path
    putStrLn "\n"
    putStrLn "Choose an option from the list below:\n"
    putStrLn "View, Image"
    putStrLn "Open, New Image"
    putStrLn "Darken, Make Image Darker"
    putStrLn "Lighten, Make Image Lighter"
    putStrLn "Swap, Swap Colors in Image"
    putStrLn "Noisy, Make Image Noisy"
    putStrLn "Rotate, Rotate Image 90 degrees"
    putStrLn "Border, Create Border for Image"
    putStrLn "Save, Image to New File"
    putStrLn "Strip, Swap Strips in File"
    putStrLn "Quit\n"
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
            
            dfe <- doesFileExist (tmpPath ++ ".jpg")

            if dfe then do

                let path = tmpPath ++ ".jpg"
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
                       
                        tmpPath <- fileName 0
                        copyFile path tmpPath
                        colorSwap path
                        image <- readJPG tmpPath
                        path <- fileName 0 
                        writeJPG path image
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

                                        image <- readJPG path
                                        let imageTmp = rotate image
                                        path <- fileName 0
                                        writeJPG path imageTmp
                                        putStrLn "ok all set"
                                        picture path
                                    
                                    else do 

                                        if (map toLower input) == "border" then do
                                          
                                            putStrLn "What Color Border:"
                                            putStrLn "Type in Red value:"
                                            r <- getLine
                                            putStrLn "Type in Green value:"
                                            g <- getLine
                                            putStrLn "Type in Blue value:"
                                            b <- getLine
                                            putStrLn "Thickness of the border:"
                                            n <- getLine

                                            let pixel = (RGB (read r::Int) (read g::Int) (read b::Int))
                                            image <- readJPG path
                                            let imageTmp = thickBorder image pixel (read n::Int) 
                                            path <- fileName 0
                                            writeJPG path imageTmp
                                            putStrLn "Ok all set"
                                            picture path
 
                                        else do

                                            if (map toLower input) == "strip" then do
                                            
                                                putStrLn "How many strips would you like?"
                                                n <- getLine
                                                path <- strips (read n::Int) path
                                                putStrLn "okay I did it"
                                                picture path

                                            else do
                                            
                                                putStrLn "I don't understand"
                                                picture path








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

randomImage x y = do

    image <- createImage x y 0 []

    return image

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

darken delta image = (Im (fstI image) (sndI image) (darkenP delta (trdI image))) 

darkenP delta pixels = map (\x->deltaCheck (-1 * delta) x) pixels

deltaCheck delta pixel = (RGB 
    (deltaCh delta (fstP pixel)) 
    (deltaCh delta (sndP pixel)) 
    (deltaCh delta (trdP pixel)))    
    
deltaCh value lambda = 

    if (value + lambda) > 255 then 
        255 
    else 
        if (value + lambda) < 0 then 
            0 
        else (value + lambda)


colorSwap image = (Im (fstI image) (sndI image) (swap (trdI image)))

swap pixels = map (\x->swapped x) pixels

swapped pixel = (RGB (sndP pixel) (trdP pixel) (fstP pixel))    
 
swaps :: [a] -> IO [a]
swaps x = if length x < 2 then return x else do
    i <- randomRIO (0, length(x)-1)
    r <- swaps (take i x ++ drop (i+1) x)
    return (x!!i : r)

strips n image = do

    let tmpPixels = chunksOf (fstI image) (trdI image)
    let pixels = chunksOf ((sndI image) `div` n) tmpPixels
    tmpPixels <- swaps (pixels)
    let pixels = concat (map concat tmpPixels)
    let tmpImage = Im (fstI image) (sndI image) pixels

    return tmpImage


noisy image = do

    let pixels = trdI image
    pixelNoise <- swapNoisy pixels [] 

    return (Im (fstI image) (sndI image) pixelNoise)  

swapNoisy pixels temp = do

    if pixels /= [] then do
        
        let tmpPixels = head pixels
        let tmp = tail pixels
        let pixels = tmp

        chance <- randomL [0..9]
        let c = chance
        let t = temp

        if c == 0 then do
       
            let temp = t ++ (take 1 (repeat (swapped tmpPixels)))
            swapNoisy pixels temp
 

        else do
            
            let temp = t ++ (take 1 (repeat (tmpPixels)))
            swapNoisy pixels temp 
           
    else do

        return temp

view :: Image -> IO ()
view image = do

    path <- fileName 0

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

       image <- readJPG path
       picture image

    else do

        putStrLn "The file does not exist, try again"
        pictureTime

picture image = do

    let specialChar = " !\"#$%&'()*+,-./:;<=>?@^_`{|}~"
    let charList = ['a'..'z']
    let allSpecial = specialChar ++ charList
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

        view image
        picture image

    else do

        if (map toLower input) == "open" then do

            putStrLn "Open a new image: "
            tmpPath <- getLine 
            
            dfe <- doesFileExist (tmpPath ++ ".jpg")

            if dfe then do

                let path = tmpPath ++ ".jpg"
                putStrLn "Okay I did it"
                image <- readJPG path
                picture image

            else do 

                putStr "File does not Exist, renter command"
                picture image

        else do
    
            if (map toLower input) == "save" then do

                putStrLn "What would you like to name the file"
                savePath <- getLine
                let path = (savePath ++ ".jpg")
                writeJPG path image
                cleanup
                picture image

            else do
    
                if (map toLower input) == "quit" then do

                    cleanup
                    putStrLn "Bye Bye"
                    return()

                else do 

                    if (map toLower input) == "swap" then do
                       
                        let imageTmp = colorSwap image
                        putStrLn "Ok all set"
                        cleanup
                        picture imageTmp
 
                    else do

                        if (map toLower input) == "darken" then do
                                        
                            putStrLn "By how much would you like to darken the image?"
                            input <- getLine

                            if toLower (head input) `elem` allSpecial then do

                                putStrLn "Needs an integer from 0 to 255 try again"
                                picture image

                            else do

                                let delta = (read input :: Int)
                                let imageTmp = darken delta image
                                putStrLn "Ok all set"
                                cleanup
                                picture imageTmp
 
                        else do

                            if (map toLower input) == "lighten" then do
                                
                                putStrLn "By how much would you like to lighten the image?"
                                input <- getLine

                                if toLower (head input) `elem` allSpecial then do

                                    putStrLn "Needs an integer from 0 to 255 try again"
                                    picture image

                                else do
                                
                                    let delta = (-1) * (read input :: Int)
                                    let imageTmp = darken delta image
                                    cleanup
                                    putStrLn "Ok all set"
                                    picture imageTmp
 
                            else do

                                if (map toLower input) == "noisy" then do
                                    
                                    imageTmp <- noisy image
                                    cleanup
                                    putStrLn "Ok all set"
                                    picture imageTmp
 
                                else do

                                    if (map toLower input) == "rotate" then do
                            
                                        let imageTmp = rotate image
                                        cleanup
                                        putStrLn "ok all set"
                                        picture imageTmp
                                    
                                    else do 

                                        if (map toLower input) == "border" then do
                                          
                                            putStrLn "What Color Border:"
                                            putStrLn "Type in Red value:"
                                            r <- getLine

                                            if toLower (head r) `elem` allSpecial then do

                                                putStrLn "Needs an integer from 0 to 255 try again"
                                                picture image 

                                            else do

                                                putStrLn "Type in Green value:"
                                                g <- getLine

                                                if toLower (head g) `elem` allSpecial then do

                                                    putStrLn "Needs an integer from 0 to 255 try again"
                                                    picture image

                                                else do

                                                    putStrLn "Type in Blue value:"
                                                    b <- getLine

                                                    if toLower (head b) `elem` allSpecial then do

                                                        putStrLn "Needs an integer try again"
                                                        picture image

                                                    else do
                                            
                                                        putStrLn "Thickness of the border:"
                                                        n <- getLine

                                                        if toLower (head n) `elem` allSpecial then do

                                                            putStrLn "Needs an integer try again"
                                                            picture image

                                                        else do

                                                            let pixel = (RGB (read r::Int) (read g::Int) (read b::Int))
                                                            let imageTmp = thickBorder image pixel (read n::Int) 
                                                            cleanup
                                                            putStrLn "Ok all set"
                                                            picture imageTmp
 
                                        else do

                                            if (map toLower input) == "strip" then do
                                            
                                                putStrLn "How many strips would you like?"
                                                n <- getLine

                                                if toLower (head n) `elem` allSpecial then do

                                                    putStrLn "Needs an integer try again"
                                                    picture image

                                                else do

                                                    imageTmp <- strips (read n::Int) image
                                                    putStrLn "okay I did it"
                                                    cleanup
                                                    picture imageTmp

                                            else do
                                            
                                                putStrLn "I don't understand"
                                                picture image








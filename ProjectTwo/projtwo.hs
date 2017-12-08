{--Robert Davis--}

import Data.List.Split
import Data.List

display :: Image -> IO()
display (Im _ 0 _) = return ()
display (Im w h ps) = dr (take w ps) >> display (Im w (h-1) (drop w ps))

dr :: [Pixel] -> IO()
dr (p:ps) = (putChar (colorChar p)) >> dr ps
dr [] = putChar '\n'

colorChar :: Pixel -> Char
colorChar (RGB r g b) = "09&%#@BMNPO8GFRUTH643E$S)(][}{7I|1><lhbyzxqpuowrevac+=~-_i\"':;,." !! (div (sum [r,g,b]) 12)

data Pixel = RGB Int Int Int deriving (Show,Eq)
data Image = Im Int Int [Pixel] deriving Show

hImage :: Image
hImage = Im 16 20 [b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,g,g,g,g,g,g,g,g,g,g,g,g,g,g,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b]
    where b = RGB 0 0 0
          g = RGB 0 255 0

fir (Im a _ _) = a 
  
scd (Im _ a _) = a 
     
trd (Im _ _ a) = a 

getFirst (Im x _ _) = x
getSecond (Im _ x _) = x
getEnd x = reverse (tail (reverse x))

gFirst (a, _, _) = a
gSecond (_, a, _) = a
gThird (_, _, a) = a

firstRow :: Image -> [Pixel]
firstRow image = take (fir image) (trd image)

lastRow :: Image -> [Pixel]
lastRow image = drop (length (trd image) - (fir image)) (trd image)

dropFirstRow :: Image -> (Int, Int, [Pixel])
dropFirstRow image = (fir image , scd image, (drop (fir image) (trd image)))

dropLastRow :: Image -> (Int, Int, [Pixel]) 
dropLastRow image = (fir image, scd image, take ((length (trd image)) -(fir image)) (trd image))

firstCol :: Image -> [Pixel]
firstCol image = [ trd image !! x | x<-[0,fir image..((length (trd image)) - (fir image))]]

lastCol :: Image -> [Pixel]
lastCol image = [ trd image !! x | x<-[(fir image - 1), ((2 *fir image)- 1)..(length (trd image)-1) ]]

dropFirstRows :: Image -> Int -> Image
dropFirstRows image n = (Im (fir image) (scd image - n) (drop ((fir image) * n) (trd image)))

dropLastRows :: Image -> Int -> Image
dropLastRows image n = (Im (fir image) (scd image - n) (take ((length (trd image)) - ((fir image) * n)) (trd image)))

rowStackFirst :: [Pixel] -> Image -> Image
rowStackFirst stack image = (Im (fir image) (scd image + 1) (stack ++ trd image))

rowStackLast :: [Pixel] -> Image -> Image
rowStackLast stack image = (Im (fir image) (scd image + 1) (trd image ++ stack))

rowSandwich :: [Pixel] -> Image -> (Int, Int, [Pixel])
rowSandwich stack image = (fir image, (scd image) + 2, stack ++ trd image ++ stack) 

vStack :: Image -> Image -> (Int, Int, [Pixel])
vStack imageOne imageTwo = (fir imageOne, (scd imageOne+ scd imageTwo), trd imageOne ++ trd imageTwo)

vTile :: Int -> Image -> (Int, Int, [Pixel])
vTile n images = ((fir images), ((scd images) * n), concat (replicate n (trd images)))

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

colSandwich :: [Pixel] -> Image -> Image
colSandwich x y = (colStackFirst x (colStackLast x y))

image :: Int -> Image -> [[Pixel]]
image n x = concat(take n (repeat(transpose (blocks x))))   

f :: [[Pixel]] -> Image -> Image
f [] y = y
f (x:xs) y = (f xs (colStackLast x y)) 

hTile :: Int -> Image -> Image
hTile n x = f (image n x) x

tile :: Int -> Int -> Image -> Image
tile n m x = (Im (gFirst(vTile n (hTile (m-1) x))) 
    (gSecond(vTile n (hTile (m-1) x))) (gThird(vTile n (hTile (m-1) x))))

border :: Pixel -> Image -> Image
border x y = 
    (colStackLast  (take (getSecond y + 2) (repeat (x)))
    (colStackFirst (take (getSecond y + 2) (repeat (x))) 
    (rowStackLast  (take (getFirst y) (repeat (x))) 
    (rowStackFirst (take (getFirst y) (repeat (x))) y))))

thickBorder :: (Eq t, Num t) => Image -> Pixel -> t -> Image
thickBorder x y 0     = x 
thickBorder x y times = thickBorder (border (y) x) y  (times - 1)

crop0 x 0  = x 
crop0 x a  = crop0 (dropFirstCol x) (a - 1)

crop1 x d = x
crop1 x d = crop1 (dropLastCol x) (getSecond(x) - 1)

crop2 x b = dropFirstRows x b 

crop3 x c = dropLastRows  x (getFirst (x) - c)  

crop :: (Num t, Eq t) => t -> Int -> Int -> Int -> Image -> Image
crop a b c d x = (crop3 (crop2 (crop1 (crop0 x a) d) b) c)

rotateCW :: [[a]] -> [[a]]
rotateCW = map reverse . transpose

rotateCCW :: [[a]] -> [[a]]
rotateCCW = reverse . transpose

rotate180 :: [[a]] -> [[a]]
rotate180 = rotateCW . rotateCW


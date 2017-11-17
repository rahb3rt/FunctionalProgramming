-- Robert Davis; Project One

display :: (Int,Int,[Bool]) -> IO()
display (_,0,_) = return ()
display (w,h,ps) = dr (take w ps) >> display (w,h-1, drop w ps)

dr :: [Bool] -> IO()
dr [] = putChar '\n'
dr (p:ps) = (putChar (if p then '0' else '.')) >> dr ps

blackImage :: Int -> Int -> (Int, Int, [Bool]) 
blackImage n m = (n, m, take (n * m) (repeat False))

whiteImage :: Int -> Int -> (Int, Int, [Bool])
whiteImage n m = (n, m, take (n * m) (repeat True))

fir :: (a, b, c) -> a 
fir (Im a _ _) = a

scd :: (a, b, c) -> b 
scd (Im _ a _) = a

trd :: (a, b, c) -> c
trd (Im _ _ a) = a

firstRow :: (Int, b, [a]) -> [a] 
firstRow image = take (fir image) (trd image)

lastRow :: (Int, b, [a]) -> [a]
lastRow image = drop (length (trd image) - (fir image)) (trd image) 

dropFirstRow :: (Int, b, [a]) -> (Int, b, [a]) 
dropFirstRow image = (fir image , scd image, (drop (fir image) (trd image)))

dropLastRow :: (Int, b, [a]) -> (Int, b, [a])
dropLastRow image = (fir image, scd image, take ((length (trd image)) -(fir image)) (trd image))

firstCol :: (Int, b, [a]) -> [a]
firstCol image = [ trd image !! x | x<-[0,fir image..((length (trd image)) - (fir image))]]

lastCol :: (Int, b, [a]) -> [a] 
lastCol image = [ trd image !! x | x<-[(fir image - 1), ((2 *fir image)- 1)..(length (trd image)-1) ]] 

dropFirstRows :: (Int, b, [a]) -> Int -> (Int, b, [a])
dropFirstRows image n = (fir image, scd image, (drop ((fir image) * n) (trd image)))

dropLastRows :: (Int, b, [a]) -> Int -> (Int, b, [a])
dropLastRows image n = (fir image, scd image, take ((length (trd image)) - ((fir image) * n)) (trd image))  

rowStackFirst :: Num b => [a1] -> (a2, b, [a1]) -> (a2, b, [a1])
rowStackFirst stack image = (fir image, (scd image) + 1, stack ++ trd image)

rowStackLast :: Num b => [a1] -> (a2, b, [a1]) -> (a2, b, [a1]) 
rowStackLast stack image = (fir image, (scd image) + 1, trd image ++ stack)

rowSandwich :: Num b => [a1] -> (a2, b, [a1]) -> (a2, b, [a1])
rowSandwich stack image = (fir image, (scd image) + 2, stack ++ trd image ++ stack) 

vStack :: Num b => (a1, b, [a2]) -> (a3, b, [a2]) -> (a1, b, [a2])
vStack imageOne imageTwo = (fir imageOne, (scd imageOne+ scd imageTwo), trd imageOne ++ trd imageTwo)

vTile :: Int -> (Int, Int, [bool]) -> (Int, Int, [bool])
vTile n images = ((fir images), ((scd images) * n), concat (replicate n (trd images)))

hImage :: (Int,Int,[Bool])
hImage =(16,20,[True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,      True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,         False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,         False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,         False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,         True,True,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,     False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,         False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,         False,False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,True,False,False,False,False,False,False,False,False,False,         False,False,False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,True,True,True,True,True,True,True,True,True,True,True,       True,True,True,True,True])

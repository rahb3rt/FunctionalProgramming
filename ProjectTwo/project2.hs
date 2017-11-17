display :: Image -> IO()
display (Im _ 0 _) = return ()
display (Im w h ps) = dr (take w ps) >> display (Im w (h-1) (drop w ps))

dr :: [Pixel] -> IO()
dr (p:ps) = (putChar (colorChar p)) >> dr ps
dr [] = putChar '\n'

colorChar :: Pixel -> Char
colorChar (RGB r g b) = "09&%#@BMNPO8GFRUTH643E$S)(][}{7I|1><lhbyzxqpuowrevac+=~-_i\"':;,." !! (div (sum [r,g,b]) 12)

thumb :: Image
thumb = Im 16 20 [b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,w,w,w,w,w,w,w,w,w,w,w,w,w,w,b,b,w,w,w,w,w,w,w,w,w,w,w,w,w,w,b,b,w,w,w,w,d,d,w,w,w,w,w,w,w,w,b,b,w,w,w,d,l,d,w,w,w,w,w,w,w,w,b,b,w,w,w,d,p,p,d,w,w,w,w,w,w,w,b,b,w,w,w,d,p,p,d,w,w,w,w,w,w,w,b,b,w,w,w,d,p,d,w,w,w,w,w,w,w,w,b,b,w,w,w,d,p,p,d,d,d,d,d,w,w,w,b,b,w,w,w,d,p,p,p,d,l,p,p,d,w,w,b,b,w,d,d,d,p,p,p,p,d,d,d,w,w,w,b,b,w,p,p,p,p,p,p,d,l,p,p,d,w,w,b,b,w,p,p,p,p,p,p,p,d,d,d,w,w,w,b,b,w,p,p,p,p,p,p,d,l,p,p,d,w,w,b,b,w,d,d,d,p,p,p,p,d,d,d,w,w,w,b,b,w,w,w,d,p,p,p,d,l,p,d,w,w,w,b,b,w,w,w,w,d,d,d,d,d,d,w,w,w,w,b,b,w,w,w,w,w,w,w,w,w,w,w,w,w,w,b,b,w,w,w,w,w,w,w,w,w,w,w,w,w,w,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b]
 where w = RGB 255 255 255
       b = RGB 0 0 0
       d = RGB 161 122 31
       l = RGB 236 139 174
       p = RGB 254 194 48

data Pixel = RGB Int Int Int deriving (Show,Eq)
data Image = Im Int Int [Pixel] deriving Show

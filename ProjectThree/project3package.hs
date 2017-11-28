module Project3package where

import qualified Codec.Picture as CP
import qualified Codec.Picture.Types as CPT
import qualified Data.Vector.Storable as DVS
import qualified Data.ByteString.Lazy as DBL

data Pixel = RGB Int Int Int deriving (Show,Eq)
data Image = Im Int Int [Pixel] deriving (Show,Eq)

listToTrips :: [Int] -> [Pixel]
listToTrips (a:b:c:ds) = (RGB a b c):(listToTrips ds)
listToTrips _ = []

tripsToList :: [Pixel] -> [Int]
tripsToList [] = []
tripsToList ((RGB a b c):ts) = a:b:c:(tripsToList ts)

p8im :: Image -> CP.Image CP.PixelRGB8
p8im (Im w h ts) = CPT.Image w h (DVS.fromList (map fromIntegral (tripsToList ts)))

readJPG :: FilePath -> IO Image
readJPG fp = do
    Right i <- CP.readImage fp
    let j = CP.convertRGB8 i
    return $ Im (CPT.imageWidth j) (CPT.imageHeight j) (listToTrips (map fromIntegral (DVS.toList (CPT.imageData j))))

writeJPG :: FilePath -> Image -> IO ()
writeJPG fp i = DBL.writeFile fp (CP.encodeJpegAtQuality 100 (CPT.convertImage (p8im i)))

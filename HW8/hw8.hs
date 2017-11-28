import Text.Printf
import Data.Char 

data Term = Years Int | Life deriving Show


instance Eq Term where
    (Years a ) == (Years b ) = a == b

instance Ord Term where
    compare (Years a) (Years b) = compare a b


mpg :: IO()
mpg = do
  putStrLn "Miles Driven?"
  s<- getLine
  putStrLn "Gallons Used?"
  p<- getLine

  printf "You drove %0.2f miles per gallon\n"  ((read s :: Float)/(read p :: Float)) 

greeter :: IO()
greeter = do
    putStrLn "Whats you name?"
    s <- getLine
    if (length s < 5) then
        do
            putStrLn "That's a short name!"
            
        else do
            putStrLn "That's a long name!"

shout :: IO ()
shout = do
  putStrLn "Type something in and I will shout it."
  s <- getLine
  if (length s == 0) then do
    
	putStrLn "Bye!"

  else do
    
  let p = (map toUpper s);
  putStrLn $ p;
  shout


shouter :: IO ()
shouter = do

  putStrLn "Welcome to the shouter!"
  shout

calc :: IO ()
calc = do
    putStrLn "WELCOME!"
    calch 0 0
    
calch :: (Show t, Num t, Read t) => t -> t -> IO ()	
calch t m = do
    putStrLn $ "Total is " ++ show t
    s <- getLine
    calcProcess s t m

calcProcess :: (Show t, Num t, Read t) => String -> t -> t -> IO ()
calcProcess s t m
 | take 2 s == "+ "         = do
                                let newt = t + read (drop 2 s)
                                calch newt m
 | take 2 s == "* "         = calch (t * read (drop 2 s)) m
 | map toLower s == "store" = do
 								let m = t
								calch t m
 | map toLower s == "retrieve" = do 
 								 calch m m
 | map toLower s == "quit"  = putStrLn "Bye!"
 | otherwise                = do
                                putStrLn "Input error"


numList :: [Int] -> IO [Int]
numList numbers = do
    putStrLn "Gimme a number!"
    n <- getLine
    case n of
        "" -> pure numbers
        _ -> case reads n of
            [(number, "")] -> numList (number : numbers)
            _ -> numList numbers

evenOddCount :: IO ()
evenOddCount = do
  numbers <- numList []
  let m = length numbers
  let e = length [ x | x <- numbers, even x ]
  let o = m - e
  printf "You gave me %d evens and %d odds!\n" e o


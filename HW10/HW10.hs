import System.Random
import Data.List
import Data.List.Split

randomL :: [b] -> IO b 
randomL l = do
    seed <- newStdGen
    let i = fst (randomR (0,length l-1) seed)
    return $ l!!i

randomLine :: FilePath -> IO [[Char]]
randomLine file = do
    f <- readFile file
    let file = lines f
    f <- swaps file
    let file = take 1 f
    return file

secretNumber :: (Show a, Ord a, Enum a, Num a) => a -> IO ()
secretNumber x = do 

    n <- secrets x

    putStrLn n

secrets :: (Num a, Enum a, Ord a, Show a) => a -> IO [Char]
secrets x = do

    number <- randomL [1..100]
    let secret = number

    if x > secret then do
        
        return ("Too big! the secret number was " ++ show secret)
    
    else  do
        
        if  x < secret then do
            
            return ("Too small! The secret number was " ++ show secret)

        else do
            
            return ("Right!") 

takeR :: Int -> [a] -> IO [a]
takeR n x = do 

    list <- swaps x
    let m = take n list
    return m

swaps :: [a] -> IO [a]
swaps x = if length x < 2 then return x else do
    i <- randomRIO (0, length(x)-1)
    r <- swaps (take i x ++ drop (i+1) x)
    return (x!!i : r)

dropR :: (Eq a, Num b, Ord b) => b -> [a] -> IO b
dropR n x = do

    list <- removes n x 0
    
    return list

removes :: (Eq a, Ord b, Num b) => b -> [a] -> b -> IO b
removes n x i = do

    if i < n then do

        number <- swaps x
        let del = head number
        let list = delete del number
        let j = i
        let i = j + 1
        removes n list i

    else do

        return n

generatePassword :: (Eq t, Num t) => t -> IO [Char]
generatePassword 0 = return ""
generatePassword n = do
    p <- randomL (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    rs <- generatePassword (n-1)
    return $ p:rs

password :: (Num t, Eq t) => t -> IO [Char]
password n = do 

    pass <- generatePassword n
    let check = (filter (==True) (map (\x-> elem x pass) ['0'..'9']))

    if check == [] then do

        password n

    else do

        if head check /=  True || check == [] then do 

            password n

        else do 

            return pass

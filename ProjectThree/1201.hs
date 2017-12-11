import System.Random

randomL :: [a] -> IO a
randomL l = do
    seed <- newStdGen
    let i = fst (randomR (0,length l-1) seed)
    return $ l!!i

insert x [] = [x] 
insert x (y:ys) = if x <= y
                  then x:y:ys
                  else y : insert x ys

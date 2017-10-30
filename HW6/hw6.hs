compy :: Integral b => b -> [b]
compy n = map ((\n->n^2-n*4)) (filter (\n -> n `mod` 4 == 1) [1..n])

collatz :: Int -> Int
collatz x = if (even x) then (div x 2) else (3*x+1)

collatzSequence :: Int -> [Int]
collatzSequence 1 = []
collatzSequence x = (collatz x) : (collatzSequence (collatz x))

--collatzN :: Int -> [Int]
--collatzN n = take 1 (filter (\x->length (collatzSequence x) == n)[1..])

--Got bored, did both version hope I get extra credit ;)--
collatzN :: Int -> Int
collatzN n =  length (fst (break (n ==) (map length (map collatzSequence [1..])))) + 1

deVowel :: [Char] -> [Char]
deVowel n = filter (\n ->elem n "bcdfghjklmnpqrstvwxyz") n

excluder :: (Foldable t, Eq a) => t a -> [a] -> [a]
excluder x y = filter (\n->notElem n x) y

intersection :: (Foldable t, Eq a) => t a -> [a] -> [a]
intersection x y = filter (\n->elem n x) y

maxxer :: (Ord a, Foldable t) => [t a] -> a
maxxer = maximum . map maximum

startsWithOne :: (Eq a, Num a) => [a] -> Bool
startsWithOne = (1 ==) . head

slowTalk :: [a] -> [a] 
slowTalk n = concat (map (replicate 2) n)


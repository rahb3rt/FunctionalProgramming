import Data.Char

supyo :: IO ()
supyo = do
    putStrLn "sup"
    putStrLn "yo"
    
greeter :: IO()
greeter = do
    putStrLn "What's your name?"
    s <- getLine
    putStrLn $ "Hello, " ++ s ++ "!"
    
fancyGreeter :: IO()
fancyGreeter = do
    putStrLn "What's your name?"
    s <- getLine
    putStrLn "What is your quest?"
    q <- getLine
    let bigQ = map toUpper q
    putStrLn $ "Good luck with that " ++ bigQ ++ ", " ++ s
    
bouncer :: IO()
bouncer = do
    putStrLn "How old are you?"
    s <- getLine
    if (read s >= 18) then
        do
            putStrLn "Great!"
            putStrLn "Come right in"
        else do
            putStrLn "Get out!"
    
insulter :: IO()
insulter = do
    putStrLn "Your an idiot"
    s <- getLine
    if (map toLower s == "jellybean") then
        do
            return ()
        else do
            insulter
            putStrLn $ "Only idiots say " ++ s
    
    
    
    
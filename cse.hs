import System.IO
import System.Environment 

data Ninja = Ninja {name :: String, country :: Char,
                    status :: String, exam1 :: Float,
                    exam2 :: Float, ability1 :: String,
                    ability2 :: String, r :: Int}
                    deriving (Show)


fire :: [Ninja] -- add the junior ninjas of Land of Fire to that list
fire = []
lightning :: [Ninja] -- add the junior ninjas of Land of Lightning to that list
lightning = []
water :: [Ninja] -- add the junior ninjas of Land of Water to that list
water = []
wind :: [Ninja] -- add the junior ninjas of Land of Wind to that list
wind = []
earth :: [Ninja] -- add the junior ninjas of Land of Earth to that list
earth = []

--empty functions (these functions will be implemented)
allCountriesNinjaInfo = putStrLn "empty function"
roundBetweenNinjas = putStrLn "empty function"
roundBetweenCountries = putStrLn "empty function"
exit = putStrLn "empty function"

-- option (a) from menu
-- fills the list of given country, sorts the list and prints it (if there is Journeyman, it gives warning)
aCountryNinjaInfo :: [[[Char]]] -> IO ()
aCountryNinjaInfo list = do
    putStr "Enter the country code: "
    hFlush stdout
    country <- getLine
    case country of
        x | x `elem` ["e", "E"] -> (mapM_ putStrLn $ printList $ sortBy 1 $ sortBy 0 (append earth "Earth")) >> warning (append earth "Earth") "Earth"
        x | x `elem` ["l", "L"] -> (mapM_ putStrLn $ printList $ sortBy 1 $ sortBy 0 (append lightning "Lightning")) >> warning (append lightning "Lightning") "Lightning"
        x | x `elem` ["w", "W"] -> (mapM_ putStrLn $ printList $ sortBy 1 $ sortBy 0 (append water "Water")) >> warning (append water "Water") "Water"
        x | x `elem` ["n", "N"] -> (mapM_ putStrLn $ printList $ sortBy 1 $ sortBy 0 (append wind "Wind")) >> warning (append wind "Wind") "Wind"
        x | x `elem` ["f", "f"] -> (mapM_ putStrLn $ printList $ sortBy 1 $ sortBy 0 (append fire "Fire")) >> warning (append fire "Fire") "Fire"
        _ -> putStrLn "unknown country code"
        where
            append list' country' = list' ++ (fillList list country')
            warning list'' country'' = (if length (filter (\x' -> status x' == "Journeyman") list'') == 1 then putStrLn (country'' ++ " country cannot be included in a fight" ++ "\n") else putStrLn "")
    

--menu is displayed to user
menu :: [[[Char]]] -> IO ()
menu list = do
    putStrLn "a) View a Country's Ninja Information"
    putStrLn "b) View All Countries' Ninja Information"
    putStrLn "c) Make a Round Between Ninjas"
    putStrLn "d) Make a Round Between Countries"
    putStrLn "e) Exit"
    putStr "Enter the action: "
    hFlush stdout
    choice <- getLine
    callTheFunction list choice
    if choice `elem` ["e", "E"]
        then return ()
        else do 
            menu list
            
-- menu calls the proper function based on the user selection
callTheFunction :: [[[Char]]] -> String -> IO()
callTheFunction list choice'
  | choice' `elem` ["a", "A"] = aCountryNinjaInfo list
  | choice' `elem` ["b", "B"] = allCountriesNinjaInfo
  | choice' `elem` ["c", "C"] = roundBetweenNinjas
  | choice' `elem` ["d", "D"] = roundBetweenCountries
  | choice' `elem` ["e", "E"] = exit
  | otherwise = putStrLn "unknown option"         


-- After reading file, list contains file content line by line, this function is called to split these lines to features
splitFeatures :: [[Char]] -> [[[Char]]]
splitFeatures list@(l:ls) = map tokenize list
    where 
        tokenize :: [Char] -> [[Char]]
        tokenize l' = case break (==' ') l' of
            (newList', "") -> [newList']
            (newList', ll:lls) -> newList' : tokenize lls


-- function takes an element from list (list contains all ninjas' features like [[f11, f12,..], [f21, f22, ..], ...])
-- and creates ninja objects then gives a list that contains these objects
fillList :: [[[Char]]] -> [Char] -> [Ninja]
fillList newList country = [newNinja x | x <- list]
    where
        list = filter (\x' -> (x' !! 1) == country) newList
        newNinja x = Ninja {name = (x !! 0) , country = head (x !! 1), status = "Junior" , exam1 = (read (x !! 2) :: Float), 
                    exam2 = (read (x !! 3) :: Float), ability1 = (x !! 4), ability2 = (x !! 5), r = 0 }

-- ability impacts
abilityTable :: String -> Float
abilityTable ability = case ability of
    "Clone" -> 20.0
    "Hit" -> 10.0
    "Lightning" -> 50.0
    "Vision" -> 30.0
    "Sand" -> 50.0
    "Fire" -> 40.0
    "Water" -> 30.0
    "Blade" -> 20.0
    "Summon" -> 50.0
    "Storm" -> 10.0
    "Rock" -> 20.0

-- print a country list with desired format
printList :: [Ninja] -> [String]
printList list = [format x | x <- list]
    where 
        format x = name x ++ ", Score: " ++ show (calculateScore (exam1 x) (exam2 x) (abilityTable $ ability1 x) (abilityTable $ ability2 x)) ++ ", Status: " ++ status x ++ ", Round: " ++ show (r x)
    
-- calculate ninja score based on exams and abilities points
calculateScore :: Float -> Float -> Float -> Float -> Float
calculateScore e1 e2 a1 a2 = 0.5 * e1 + 0.3 * e2 + a1 + a2

-- quick sort is used to sort country lists
sortBy :: Integer -> [Ninja] -> [Ninja]
sortBy _ [] = []
sortBy 0 (x:xs) = sortBy 0 small ++ [x] ++ sortBy 0 large
    where
        small = [a | a <- xs, score a <= score x]
        large = [b | b <- xs, score b > score x]
        score x' = calculateScore (exam1 x') (exam2 x') (abilityTable $ ability1 x') (abilityTable $ ability2 x')
sortBy 1 (x:xs) = sortBy 1 small ++ [x] ++ sortBy 1 large
    where
        small = [a | a <- xs, r a <= r x]
        large = [b | b <- xs, r b > r x]

main = do 
    --takes file name from command line
    fileName <- getArgs
    --putStrLn $ head fileName

    -- read file
    fileContent <- readFile $ head fileName

    -- create a list from file content (each element will be a line)
    let list = splitFeatures $ lines fileContent
    --print list
    --print $ fire ++ (fillList list "Fire")
    --mapM_ putStrLn $ printList (fire ++ (fillList list "Fire")) --can be converted to higher order function 
    menu list
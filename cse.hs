import System.IO
import System.Environment
import Prelude

data Ninja = Ninja {name :: String, country :: Char,
                    status :: String, exam1 :: Float,
                    exam2 :: Float, ability1 :: String,
                    ability2 :: String, r :: Int, score:: Float}

        
instance Show Ninja where 
    show (Ninja name _ status _ _ _ _ r score) = name ++ ", " ++ "Score: " ++ (show score) ++ " Status: " ++ status ++ ", Round: " ++ (show r)

instance Eq Ninja where
    (==) ninja1  ninja2 = name ninja1 == name ninja2

instance Ord Ninja where 
    compare ninja1  ninja2
        | r ninja1 == r ninja2 = case () of
            ()| score ninja1  <= score ninja2  -> GT
              | otherwise -> LT
        | r ninja1 <= r ninja2 = LT
        | otherwise = GT

(<?>) :: Ninja -> Ninja -> Bool
(<?>) ninja1 ninja2 
        | score ninja1 > score ninja2 = True
        | score ninja1 < score ninja2 = False
        | otherwise = case () of
                ()| a11 + a12 > a21 + a22 -> True
                  | a11 + a12 < a21 + a22  -> False
                  | otherwise -> True -- should be replaced with random
                    where
                        a11 = (abilityTable $ ability1 ninja1)
                        a12 = (abilityTable $ ability2 ninja1)
                        a21 = (abilityTable $ ability1 ninja2)
                        a22 = (abilityTable $ ability2 ninja2)

getCountryInfo country list
            | country `elem` ["e", "E"] = ((list !! 0), "Earth")
            | country `elem` ["l", "L"] = ((list !! 1), "Lightning")
            | country `elem` ["w", "W"] = ((list !! 2), "Water")
            | country `elem` ["n", "N"] = ((list !! 3), "Wind")
            | country `elem` ["f", "f"] = ((list !! 4), "Fire")
            |otherwise = ([], "Unknown country code")

checkJourneyMan list  = length (filter (\x -> status x == "Journeyman") list) == 1

roundBetweenCountries :: [[Ninja]] -> [Ninja] -> [Ninja]  -> IO ()
roundBetweenCountries allNinjas list1 list2 =
    do 
        putStr "Enter the first country code: " >> hFlush stdout
        ninjaCountry1 <- getLine
        let (list1, county1) = getCountryInfo ninjaCountry1 allNinjas
        if null list1 
            then
                putStr "Unknown country code" >> roundBetweenCountries allNinjas [] []
            else do
                if checkJourneyMan list1 
                    then putStrLn (county1 ++ "country cannot be included in a fight" ++ "\n") >> roundBetweenCountries allNinjas [] []
                    else  do
                        putStr "Enter the second country code: " >> hFlush stdout
                        ninjaCountry2 <- getLine
                        let (list2, county2) = getCountryInfo ninjaCountry2 allNinjas
                        if null list2 
                            then
                                putStr "Unknown country code" >> roundBetweenCountries allNinjas list1 []
                            else do
                                if checkJourneyMan list2 
                                    then putStrLn (county2 ++ " country cannot be included in a fight" ++ "\n") >> roundBetweenCountries allNinjas [] []
                                    else do
                                        let ninja1 = head $ sortBy list1
                                        let ninja2 = head $ sortBy list2
                                        let res = ninja1 <?> ninja2
                                        let (newList,printWinner) = updateList res list1 list2  ninja1 ninja2 allNinjas
                                        printWinner
                                        menu newList

fire, lightning, water, wind, earth   :: [Ninja] -- add the junior ninjas of Land of Fire to that list
fire = []
lightning = []
water = []
wind = []
earth = []

--empty functions (these functions will be implemented)
exit = putStrLn "empty function"

-- option (a) from menu
-- fills the list of given country, sorts the list and prints it (if there is Journeyman, it gives warning)
aCountryNinjaInfo :: [[Ninja]] -> IO ()
aCountryNinjaInfo list = do
    putStr "Enter the country code: "
    hFlush stdout
    country <- getLine
    let (l, countryName) = getCountryInfo country list
    let warning = if checkJourneyMan l then countryName ++ " country cannot be included in a fight" ++ "\n" else ""
    if null l then putStrLn  countryName else  mapM_ putStrLn . printList $ sortBy l
    putStrLn warning
    menu list

-- option (b) from menu
allCountriesNinjaInfo ::[[Ninja]] -> IO ()
allCountriesNinjaInfo list = do
    let allCountries = (earth ++ (list !! 0)) ++ (lightning ++ (list !! 1)) 
                        ++ (water ++ (list !! 2)) ++ (wind ++ (list !! 3)) ++ (fire ++ (list !! 4))
    mapM_ putStrLn . printList $ sortBy allCountries
    putStrLn ""
    menu list   


--Updates and Notes to my group friends: 
-- For now, "roundBetweenNinjas" function only takes the desired ninjas from the list and prints the result of the match as true/false.
-- Perhaps the function can be written more effectively and in a shorter way????
-- Naruto and naruto is considered as different names according to this implementation. Is it OK or not?
-- Also I change the type of the list using from menu options
-- previous type: [[[Char]]], new type: [[Ninja]], I think that way will be more efficient for "roundBetweenNinjas" function ???

-- New Updates:
-- roundBetweenNinjas function is completed
-- each option from the menu calls menu again (previously menu function was calling itself)
-- instance Ord is fixed.
-- maybe functions can be written more effectively ???

-- option (c) from menu
roundBetweenNinjas :: [[Ninja]] -> IO ()
roundBetweenNinjas list  = do

    putStr "Enter the name of the first ninja: " >> hFlush stdout
    ninjaName1 <- getLine
    putStr "Enter the country code of the first ninja: " >> hFlush stdout
    ninjaCountry1 <- getLine

    let (list1, country1) = getCountryInfo ninjaCountry1 list
    if checkJourneyMan list1 
        then putStrLn (country1 ++ " country cannot be included in a fight" ++ "\n") >> roundBetweenNinjas list
        else  do
            if null list1 then putStrLn "Unknown country code" >> roundBetweenNinjas list
                else do
                    let ninja1 = filter (\x' -> name x' == ninjaName1) list1
                    if null ninja1 then putStrLn "There is no such ninja" >> roundBetweenNinjas list
                    else do
                        putStr "Enter the name of the second ninja: " >> hFlush stdout
                        ninjaName2 <- getLine
                        putStr "Enter the country code of the second ninja: " >> hFlush stdout
                        ninjaCountry2 <- getLine

                        let (list2, country2) = getCountryInfo ninjaCountry2 list
                        if checkJourneyMan list2 
                            then putStrLn (country2 ++ " country cannot be included in a fight" ++ "\n") >> roundBetweenNinjas list
                            else  do
                                if null list2 then putStrLn "unknown country code" >> roundBetweenNinjas list
                                else do
                                    let ninja2 = filter (\x' -> name x' == ninjaName2) list2
                                    if null ninja2 then putStrLn "There is no such ninja" >> roundBetweenNinjas list
                                    else do 
                                        let (newList,printWinner) = (updateList ((<?>) (head ninja1)  (head ninja2)) list1 list2 (head ninja1) (head ninja2) list)
                                        printWinner
                                        menu newList

-- function returns updated list (winner ninja will be updated and defeated ninja will be removed)
-- also function prints the winner ninja
updateList :: Bool -> [Ninja] -> [Ninja] -> Ninja -> Ninja -> [[Ninja]] -> ([[Ninja]], IO())
updateList result list1 list2 ninja1 ninja2 list
    | result == True    = (update2 list2 ninja2 (update1 list1 ninja1), printWinner $ updateNinja ninja1)
    | otherwise         = (update2 list1 ninja1 (update1 list2 ninja2), printWinner $ updateNinja ninja2)
    where
        update1 l1 n1 = replace' l1 ((filter (/=n1) l1) ++ [updateNinja n1]) list
        update2 l2 n2 list' = replace' l2 (filter (/=n2) l2) list'
        updateNinja n1' = (if r n1' + 1 == 3 then n1'{score = score n1' + 10, r = 3, status = "Journeyman"}
                        else n1'{score = score n1' + 10, r = r n1' + 1})
        printWinner n1' = putStrLn ("Winner: \"" ++ name n1' ++ ", Round: " ++ show (r n1') ++ ", Status: " ++ status n1' ++ "\"\n")


-- function is used to replace an element ([Ninja]) with the new updated element from our list ([[Ninja]])
replace' :: Eq t => t -> t -> [t] -> [t]
replace' a b list= map (\x -> if (a == x) then b else x) list

--menu is displayed to user
menu :: [[Ninja]] -> IO ()
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
    {-
    if choice `elem` ["e", "E"]
        then return ()
        else do 
            menu list
    -}        
-- menu calls the proper function based on the user selection
callTheFunction :: [[Ninja]] -> String -> IO()
callTheFunction list choice'
  | choice' `elem` ["a", "A"] = aCountryNinjaInfo list
  | choice' `elem` ["b", "B"] = allCountriesNinjaInfo list
  | choice' `elem` ["c", "C"] = roundBetweenNinjas list
  | choice' `elem` ["d", "D"] = roundBetweenCountries list [] []
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


-- function creates a list that contains all ninjas from five countries
-- this list will be transferred between menu options
allCountryLists :: [[[Char]]] -> [[Ninja]]
allCountryLists list = (earth ++ (fillList list "Earth")) : (lightning ++ (fillList list "Lightning")) 
                            : (water ++ (fillList list "Water")) : (wind ++ (fillList list "Wind")) 
                            : (fire ++ (fillList list "Fire")) : []


-- function takes an element from list (list contains all ninjas' features like [[f11, f12,..], [f21, f22, ..], ...])
-- and creates ninja objects then gives a list that contains these objects
fillList :: [[[Char]]] -> [Char] -> [Ninja]
fillList newList country = [newNinja x | x <- list]
    where
        list = filter (\x' -> (x' !! 1) == country) newList
        newNinja x = Ninja {name = (x !! 0) , country = head (x !! 1), status = "Junior" , exam1 = e1, 
                    exam2 = e2, ability1 = a1, ability2 = a2, r = 0,
                    score = (calculateScore e1 e2 (abilityTable a1) (abilityTable a2))}
                        where
                            e1 = (read (x !! 2) :: Float)
                            e2 = (read (x !! 3) :: Float)
                            a1 = (x !! 4)
                            a2 = (x !! 5)

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
printList list = [show x | x <- list]
    
-- calculate ninja score based on exams and abilities points
calculateScore :: Float -> Float -> Float -> Float -> Float
calculateScore e1 e2 a1 a2 = 0.5 * e1 + 0.3 * e2 + a1 + a2

-- quick sort is used to sort country lists
sortBy :: [Ninja] -> [Ninja]
sortBy [] = []
sortBy (x:xs) = sortBy small ++ [x] ++ sortBy large
    where
        small = [a | a <- xs, a <= x]
        large = [b | b <- xs, b > x]


main = do 
    --takes file name from command line
    fileName <- getArgs
    --putStrLn $ head fileName

    -- read file
    fileContent <- readFile $ head fileName

    -- create a list from file content (each element will be a line)
    let list = allCountryLists $ splitFeatures $ lines fileContent
    --print list 
    menu list

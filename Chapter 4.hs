-- HC4T1 - weatherReport with pattern matching
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

mainHC4T1 :: IO ()
mainHC4T1 = do
    putStrLn "HC4T1 - weatherReport:"
    print (weatherReport "sunny")
    print (weatherReport "cloudy")
    print (weatherReport "windy")


-- HC4T2 - dayType using pattern matching
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"

mainHC4T2 :: IO ()
mainHC4T2 = do
    putStrLn "\nHC4T2 - dayType:"
    print (dayType "Saturday")
    print (dayType "Monday")
    print (dayType "Funday")


-- HC4T3 - gradeComment using guards and matching
gradeComment :: Int -> String
gradeComment x
    | x >= 90 && x <= 100 = "Excellent!"
    | x >= 70 && x <= 89  = "Good job!"
    | x >= 50 && x <= 69  = "You passed."
    | x >= 0 && x <= 49   = "Better luck next time."
    | otherwise           = "Invalid grade"

mainHC4T3 :: IO ()
mainHC4T3 = do
    putStrLn "\nHC4T3 - gradeComment:"
    print (gradeComment 95)
    print (gradeComment 75)
    print (gradeComment 40)
    print (gradeComment 110)


-- HC4T4 - specialBirthday using pattern matching
specialBirthday :: Int -> String
specialBirthday 1  = "Happy 1st Birthday!"
specialBirthday 18 = "Congrats on adulthood!"
specialBirthday 50 = "Half a century old!"
specialBirthday _  = "Just another year!"

mainHC4T4 :: IO ()
mainHC4T4 = do
    putStrLn "\nHC4T4 - specialBirthday:"
    print (specialBirthday 1)
    print (specialBirthday 18)
    print (specialBirthday 30)


-- HC4T5 - specialBirthday with catch-all and age in message
specialBirthdayVerbose :: Int -> String
specialBirthdayVerbose 1  = "Happy 1st Birthday!"
specialBirthdayVerbose 18 = "Congrats on adulthood!"
specialBirthdayVerbose 50 = "Half a century old!"
specialBirthdayVerbose age = "You're " ++ show age ++ " years old today!"

mainHC4T5 :: IO ()
mainHC4T5 = do
    putStrLn "\nHC4T5 - specialBirthdayVerbose:"
    print (specialBirthdayVerbose 18)
    print (specialBirthdayVerbose 23)


-- HC4T6 - whatsInsideThisList using pattern matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList []       = "The list is empty."
whatsInsideThisList [_]      = "The list has one element."
whatsInsideThisList [_, _]   = "The list has two elements."
whatsInsideThisList _        = "The list has many elements."

mainHC4T6 :: IO ()
mainHC4T6 = do
    putStrLn "\nHC4T6 - whatsInsideThisList:"
    print (whatsInsideThisList ([] :: [Int]))
    print (whatsInsideThisList [1])
    print (whatsInsideThisList [1, 2])
    print (whatsInsideThisList [1, 2, 3])


-- HC4T7 - firstAndThird from list using pattern matching
firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _         = []

mainHC4T7 :: IO ()
mainHC4T7 = do
    putStrLn "\nHC4T7 - firstAndThird:"
    print (firstAndThird [10, 20, 30, 40])  -- [10, 30]
    print (firstAndThird [1, 2])            -- []
    print (firstAndThird [5, 6, 7])         -- [5, 7]


-- HC4T8 - describeTuple using pattern matching
describeTuple :: (String, Int) -> String
describeTuple (name, age) = name ++ " is " ++ show age ++ " years old."

mainHC4T8 :: IO ()
mainHC4T8 = do
    putStrLn "\nHC4T8 - describeTuple:"
    print (describeTuple ("Alice", 30))
    print (describeTuple ("Bob", 45))


-- Combined main
main :: IO ()
main = do
    mainHC4T1
    mainHC4T2
    mainHC4T3
    mainHC4T4
    mainHC4T5
    mainHC4T6
    mainHC4T7
    mainHC4T8

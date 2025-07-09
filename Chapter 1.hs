-- HC1T1 - Task 1: Function Composition

double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

mainHC1T1 :: IO ()
mainHC1T1 = do
    putStrLn "HC1T1 - doubleThenIncrement:"
    print (doubleThenIncrement 5)  -- Output: 11


-- Task 2: Pure Function Example

circleArea :: Float -> Float
circleArea r = pi * r * r

mainHC1T2 :: IO ()
mainHC1T2 = do
    putStrLn "\nTask 2 - circleArea:"
    print (circleArea 3)  -- Output: ~28.27


-- Task 3: Checking if a Number is Greater than 18

greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

mainHC1T3 :: IO ()
mainHC1T3 = do
    putStrLn "\nTask 3 - greaterThan18:"
    print (greaterThan18 20)  -- Output: True
    print (greaterThan18 10)  -- Output: False


-- Task 4: Composing a Function to Process Player Data

type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers = map fst

sortByScore :: [Player] -> [Player]
sortByScore = reverse . quicksort
  where
    quicksort [] = []
    quicksort (x:xs) =
      let lower  = [y | y <- xs, snd y <= snd x]
          higher = [y | y <- xs, snd y >  snd x]
      in quicksort higher ++ [x] ++ quicksort lower

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

mainHC1T4 :: IO ()
mainHC1T4 = do
    let players = [("Alice", 10), ("Bob", 35), ("Charlie", 25), ("Dana", 40), ("Eve", 30)]
    putStrLn "\nTask 4 - getTopThreePlayers:"
    print (getTopThreePlayers players)  -- Output: ["Dana", "Bob", "Eve"]


-- Task 5: Laziness in Haskell

infiniteNumbers :: [Int]
infiniteNumbers = [1..]

getFirstN :: Int -> [Int]
getFirstN n = take n infiniteNumbers

mainHC1T5 :: IO ()
mainHC1T5 = do
    putStrLn "\nTask 5 - getFirstN:"
    print (getFirstN 10)  -- Output: [1..10]


-- Task 6: Using Type Signatures

addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

mainHC1T6 :: IO ()
mainHC1T6 = do
    putStrLn "\nTask 6 - addNumbers:"
    print (addNumbers 7 5)  -- Output: 12


-- Task 7: Converting Fahrenheit to Celsius

fToC :: Float -> Float
fToC f = (f - 32) * 5 / 9

mainHC1T7 :: IO ()
mainHC1T7 = do
    putStrLn "\nTask 7 - fToC:"
    print (fToC 98.6)  -- Output: ~37.0


-- Task 8: Higher-Order Functions

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

mainHC1T8 :: IO ()
mainHC1T8 = do
    putStrLn "\nTask 8 - applyTwice:"
    print (applyTwice (*2) 3)  -- Output: 12


-- Combined main function to test all

main :: IO ()
main = do
    mainHC1T1
    mainHC1T2
    mainHC1T3
    mainHC1T4
    mainHC1T5
    mainHC1T6
    mainHC1T7
    mainHC1T8

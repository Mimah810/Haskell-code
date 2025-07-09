-- HC6T1: Recursive factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

mainHC6T1 :: IO ()
mainHC6T1 = do
    putStrLn "HC6T1 - factorial:"
    print (factorial 5)  -- 120
    print (factorial 0)  -- 1

-- HC6T2: Recursive Fibonacci
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

mainHC6T2 :: IO ()
mainHC6T2 = do
    putStrLn "\nHC6T2 - fibonacci:"
    print (fibonacci 0)  -- 0
    print (fibonacci 7)  -- 13

-- HC6T3: Sum using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

mainHC6T3 :: IO ()
mainHC6T3 = do
    putStrLn "\nHC6T3 - sumList:"
    print (sumList [1, 2, 3, 4])  -- 10

-- HC6T4: Product using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

mainHC6T4 :: IO ()
mainHC6T4 = do
    putStrLn "\nHC6T4 - productList:"
    print (productList [1, 2, 3, 4])  -- 24

-- HC6T5: Reverse a list using recursion
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

mainHC6T5 :: IO ()
mainHC6T5 = do
    putStrLn "\nHC6T5 - reverseList:"
    print (reverseList [1, 2, 3, 4])  -- [4,3,2,1]

-- HC6T6: Check if element exists
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists y (x:xs)
    | y == x    = True
    | otherwise = elementExists y xs

mainHC6T6 :: IO ()
mainHC6T6 = do
    putStrLn "\nHC6T6 - elementExists:"
    print (elementExists 3 [1, 2, 3, 4])  -- True
    print (elementExists 5 [1, 2, 3, 4])  -- False

-- HC6T7: List length
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

mainHC6T7 :: IO ()
mainHC6T7 = do
    putStrLn "\nHC6T7 - listLength:"
    print (listLength [1..10])  -- 10

-- HC6T8: Filter even numbers
filterEvens :: [Int] -> [Int]
filterEvens = filter even

mainHC6T8 :: IO ()
mainHC6T8 = do
    putStrLn "\nHC6T8 - filterEvens:"
    print (filterEvens [1..10])  -- [2,4,6,8,10]

-- HC6T9: Map implementation
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

mainHC6T9 :: IO ()
mainHC6T9 = do
    putStrLn "\nHC6T9 - mapList:"
    print (mapList (*2) [1,2,3])  -- [2,4,6]

-- HC6T10: Recursive function to get digits
digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

mainHC6T10 :: IO ()
mainHC6T10 = do
    putStrLn "\nHC6T10 - digits:"
    print (digits 12345)  -- [1,2,3,4,5]

-- Combined main
main :: IO ()
main = do
    mainHC6T1
    mainHC6T2
    mainHC6T3
    mainHC6T4
    mainHC6T5
    mainHC6T6
    mainHC6T7
    mainHC6T8
    mainHC6T9
    mainHC6T10

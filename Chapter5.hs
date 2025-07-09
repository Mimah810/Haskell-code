-- HC5T1: Apply function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

mainHC5T1 :: IO ()
mainHC5T1 = do
    putStrLn "HC5T1 - applyThrice:"
    print (applyThrice (+1) 5)     -- 8
    print (applyThrice (*2) 1)     -- 8


-- HC5T2: Filter odd numbers from 1 to 30
filterOdds :: [Int]
filterOdds = filter odd [1..30]

mainHC5T2 :: IO ()
mainHC5T2 = do
    putStrLn "\nHC5T2 - filterOdds:"
    print filterOdds


-- HC5T3: Check if any word starts with uppercase
hasUppercaseStart :: [String] -> Bool
hasUppercaseStart = any (\w -> not (null w) && head w `elem` ['A'..'Z'])

mainHC5T3 :: IO ()
mainHC5T3 = do
    putStrLn "\nHC5T3 - hasUppercaseStart:"
    print (hasUppercaseStart ["hello", "world"])
    print (hasUppercaseStart ["apple", "Zebra"])


-- HC5T4: Lambda for biggerThan10
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

mainHC5T4 :: IO ()
mainHC5T4 = do
    putStrLn "\nHC5T4 - biggerThan10 (lambda):"
    print (biggerThan10 8)
    print (biggerThan10 15)


-- HC5T5: Partial application to multiply by five
multiplyByFive :: Int -> Int
multiplyByFive = (*) 5

mainHC5T5 :: IO ()
mainHC5T5 = do
    putStrLn "\nHC5T5 - multiplyByFive:"
    print (multiplyByFive 3)
    print (multiplyByFive 10)


-- HC5T6: Function composition for squares of even numbers
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

mainHC5T6 :: IO ()
mainHC5T6 = do
    putStrLn "\nHC5T6 - evenSquares:"
    print (evenSquares [1..10])  -- [4, 16, 36, 64, 100]


-- HC5T7: Using the $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

mainHC5T7 :: IO ()
mainHC5T7 = do
    putStrLn "\nHC5T7 - result with $ operator:"
    print result


-- HC5T8: Point-free style
addFive :: Int -> Int
addFive = (+5)

mainHC5T8 :: IO ()
mainHC5T8 = do
    putStrLn "\nHC5T8 - addFive (point-free):"
    print (addFive 10)
    print (addFive (-5))


-- HC5T9: Apply function twice to each list element
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

mainHC5T9 :: IO ()
mainHC5T9 = do
    putStrLn "\nHC5T9 - transformList:"
    print (transformList (+1) [1, 2, 3])  -- [3,4,5]
    print (transformList (*2) [1, 2, 3])  -- [4,8,12]


-- HC5T10: Combine map, filter, any to check for squares > 50
hasBigSquare :: [Int] -> Bool
hasBigSquare = any (>50) . map (^2)

mainHC5T10 :: IO ()
mainHC5T10 = do
    putStrLn "\nHC5T10 - hasBigSquare:"
    print (hasBigSquare [1, 2, 3, 4, 5])    -- False
    print (hasBigSquare [6, 8, 10])         -- True


-- Combined main
main :: IO ()
main = do
    mainHC5T1
    mainHC5T2
    mainHC5T3
    mainHC5T4
    mainHC5T5
    mainHC5T6
    mainHC5T7
    mainHC5T8
    mainHC5T9
    mainHC5T10

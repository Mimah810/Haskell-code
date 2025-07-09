-- HC2T1 - Task 1: Checking Types in GHCi

-- Expected Types (before checking in GHCi)
-- 42                :: Num a => a       (default is Int)
-- 3.14              :: Fractional a => a (default is Double)
-- "Haskell"         :: [Char] or String
-- 'Z'               :: Char
-- True && False     :: Bool

mainHC2T1 :: IO ()
mainHC2T1 = do
    putStrLn "HC2T1 - Expected types:"
    putStrLn "42 :: Num a => a  (usually Int)"
    putStrLn "3.14 :: Fractional a => a (usually Double)"
    putStrLn "\"Haskell\" :: String"
    putStrLn "'Z' :: Char"
    putStrLn "True && False :: Bool"
    putStrLn $ "Evaluation: " ++ show (True && False)  -- Should print False

------------------------------------------------------------

-- HC2T2 - Task 2: Function Type Signatures

add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

mainHC2T2 :: IO ()
mainHC2T2 = do
    putStrLn "\nHC2T2 - Testing Functions:"
    print (add 5 7)                     -- Output: 12
    print (isEven 6)                   -- Output: True
    print (concatStrings "Hi " "There") -- Output: "Hi There"

------------------------------------------------------------

-- HC2T3 - Task 3: Immutable Variables

myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

mainHC2T3 :: IO ()
mainHC2T3 = do
    putStrLn "\nHC2T3 - Immutable Variables:"
    print myAge
    print piValue
    print greeting
    print isHaskellFun
    putStrLn "Note: Trying to modify these will result in a compile-time error."

------------------------------------------------------------

-- HC2T4 - Task 4: Infix and Prefix Conversion

-- Prefix notation:
prefix1 = (+) 5 3
prefix2 = (*) 10 4
prefix3 = (&&) True False

-- Infix notation:
infix1 = 7 + 2
infix2 = 6 * 5
infix3 = True && False

mainHC2T4 :: IO ()
mainHC2T4 = do
    putStrLn "\nHC2T4 - Prefix and Infix Conversions:"
    print prefix1
    print prefix2
    print prefix3
    print infix1
    print infix2
    print infix3

------------------------------------------------------------

-- HC2T5 - Task 5: Defining and Using Functions

circleArea2 :: Float -> Float
circleArea2 r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

mainHC2T5 :: IO ()
mainHC2T5 = do
    putStrLn "\nHC2T5 - Function Tests:"
    print (circleArea2 5)     -- 78.539816...
    print (maxOfThree 10 30 20) -- 30

------------------------------------------------------------

-- HC2T6 - Task 6: Understanding Int vs Integer

smallNumber :: Int
smallNumber = 2^62

bigNumber :: Integer
bigNumber = 2^127

-- You can try evaluating 2^64 :: Int in GHCi and it will likely overflow

mainHC2T6 :: IO ()
mainHC2T6 = do
    putStrLn "\nHC2T6 - Int vs Integer:"
    print smallNumber
    print bigNumber
    putStrLn "Note: Evaluating 2^64 :: Int may cause overflow in GHCi."

------------------------------------------------------------

-- HC2T7 - Task 7: Boolean Expressions

boolExpr1 :: Bool
boolExpr1 = True && True  -- should be True

boolExpr2 :: Bool
boolExpr2 = False || False  -- should be False

boolExpr3 :: Bool
boolExpr3 = not False  -- should be True

boolExpr4 :: Bool
boolExpr4 = 10 > 20  -- should be False

mainHC2T7 :: IO ()
mainHC2T7 = do
    putStrLn "\nHC2T7 - Boolean Expressions:"
    print boolExpr1
    print boolExpr2
    print boolExpr3
    print boolExpr4

------------------------------------------------------------

-- Combined main function
main :: IO ()
main = do
    mainHC2T1
    mainHC2T2
    mainHC2T3
    mainHC2T4
    mainHC2T5
    mainHC2T6
    mainHC2T7

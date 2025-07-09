import Text.Printf (printf)

-- HC3T1 - Check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber x = if x > 0 then "Positive"
                else if x < 0 then "Negative"
                else "Zero"

mainHC3T1 :: IO ()
mainHC3T1 = do
    putStrLn "HC3T1 - checkNumber:"
    print (checkNumber 5)
    print (checkNumber (-3))
    print (checkNumber 0)


-- HC3T2 - Determine the grade using guards
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise   = "F"

mainHC3T2 :: IO ()
mainHC3T2 = do
    putStrLn "\nHC3T2 - grade:"
    print (grade 95)
    print (grade 72)
    print (grade 50)


-- HC3T3 - Convert RGB to Hex
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    let hr = printf "%02X" r
        hg = printf "%02X" g
        hb = printf "%02X" b
    in hr ++ hg ++ hb

mainHC3T3 :: IO ()
mainHC3T3 = do
    putStrLn "\nHC3T3 - rgbToHex:"
    print (rgbToHex (255, 0, 127))  -- "FF007F"
    print (rgbToHex (0, 255, 64))   -- "00FF40"


-- HC3T4 - Triangle area using Heron's formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

mainHC3T4 :: IO ()
mainHC3T4 = do
    putStrLn "\nHC3T4 - triangleArea:"
    print (triangleArea 3 4 5)
    print (triangleArea 7 8 9)


-- HC3T5 - Determine triangle type using guards
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Scalene"

mainHC3T5 :: IO ()
mainHC3T5 = do
    putStrLn "\nHC3T5 - triangleType:"
    print (triangleType 3 3 3)
    print (triangleType 5 5 8)
    print (triangleType 6 7 8)


-- HC3T6 - Check leap year using if-then-else
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then True
    else if year `mod` 100 == 0 then False
    else if year `mod` 4 == 0 then True
    else False

mainHC3T6 :: IO ()
mainHC3T6 = do
    putStrLn "\nHC3T6 - isLeapYear:"
    print (isLeapYear 2000)  -- True
    print (isLeapYear 1900)  -- False
    print (isLeapYear 2024)  -- True


-- HC3T7 - Determine season based on month using guards
season :: Int -> String
season m
    | m == 12 || m == 1 || m == 2 = "Winter"
    | m >= 3 && m <= 5            = "Spring"
    | m >= 6 && m <= 8            = "Summer"
    | m >= 9 && m <= 11           = "Autumn"
    | otherwise                   = "Invalid month"

mainHC3T7 :: IO ()
mainHC3T7 = do
    putStrLn "\nHC3T7 - season:"
    print (season 3)   -- Spring
    print (season 7)   -- Summer
    print (season 11)  -- Autumn


-- HC3T8 - BMI using where and guards
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5 = "Underweight"
    | bmi < 25   = "Normal"
    | bmi < 30   = "Overweight"
    | otherwise  = "Obese"
  where
    bmi = weight / (height ^ 2)

mainHC3T8 :: IO ()
mainHC3T8 = do
    putStrLn "\nHC3T8 - bmiCategory:"
    print (bmiCategory 70 1.75)  -- Normal
    print (bmiCategory 90 1.8)   -- Overweight


-- HC3T9 - Find maximum of three using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z =
    let m1 = max x y
        m2 = max m1 z
    in m2

mainHC3T9 :: IO ()
mainHC3T9 = do
    putStrLn "\nHC3T9 - maxOfThree:"
    print (maxOfThree 10 20 15) -- 20
    print (maxOfThree 5 25 10)  -- 25


-- HC3T10 - Check palindrome using recursion and guards
isPalindrome :: String -> Bool
isPalindrome s
    | length s <= 1 = True
    | head s == last s = isPalindrome (init (tail s))
    | otherwise = False

mainHC3T10 :: IO ()
mainHC3T10 = do
    putStrLn "\nHC3T10 - isPalindrome:"
    print (isPalindrome "racecar")  -- True
    print (isPalindrome "haskell")  -- False
    print (isPalindrome "madam")    -- True


-- Combined main
main :: IO ()
main = do
    mainHC3T1
    mainHC3T2
    mainHC3T3
    mainHC3T4
    mainHC3T5
    mainHC3T6
    mainHC3T7
    mainHC3T8
    mainHC3T9
    mainHC3T10

{-# LANGUAGE FlexibleInstances #-}

import Text.Read (readMaybe)

-- HC7T1: Define Color and Eq instance
data Color = Red | Green | Blue deriving (Show)

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False

mainHC7T1 :: IO ()
mainHC7T1 = do
  putStrLn "HC7T1 - Eq instance:"
  print (Red == Red)    -- True
  print (Red == Blue)   -- False

-- HC7T2: Implement Ord for Color (Red < Green < Blue)
instance Ord Color where
  compare Red Red = EQ
  compare Red _ = LT
  compare Green Red = GT
  compare Green Green = EQ
  compare Green Blue = LT
  compare Blue Blue = EQ
  compare Blue _ = GT

mainHC7T2 :: IO ()
mainHC7T2 = do
  putStrLn "\nHC7T2 - Ord instance:"
  print (Red < Green)    -- True
  print (Blue > Green)   -- True
  print (Red >= Red)     -- True

-- HC7T3: compareValues with Eq and Ord constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

mainHC7T3 :: IO ()
mainHC7T3 = do
  putStrLn "\nHC7T3 - compareValues:"
  print (compareValues 5 3)          -- 5
  print (compareValues "abc" "abd")  -- "abd"
  print (compareValues Red Blue)     -- Blue

-- HC7T4: Shape type with Show and Read
data Shape = Circle Double | Rectangle Double Double deriving (Show, Read)

mainHC7T4 :: IO ()
mainHC7T4 = do
  putStrLn "\nHC7T4 - Shape Show and Read:"
  let c = Circle 5.0
      r = Rectangle 3.0 4.0
      cStr = show c
      rStr = show r
  print cStr
  print rStr
  print (read cStr :: Shape)
  print (read rStr :: Shape)

-- HC7T5: squareArea with Num constraint
squareArea :: Num a => a -> a
squareArea side = side * side

mainHC7T5 :: IO ()
mainHC7T5 = do
  putStrLn "\nHC7T5 - squareArea:"
  print (squareArea (5 :: Int))     -- 25
  print (squareArea (3.5 :: Double))-- 12.25

-- HC7T6: circleCircumference using Integral and Floating
circleCircumference :: (Integral a, Floating b) => a -> b
circleCircumference r = 2 * pi * fromIntegral r

mainHC7T6 :: IO ()
mainHC7T6 = do
  putStrLn "\nHC7T6 - circleCircumference:"
  print (circleCircumference 5)    -- approx 31.4159
  print (circleCircumference 10)   -- approx 62.8318

-- HC7T7: nextColor using Bounded and Enum
instance Bounded Color where
  minBound = Red
  maxBound = Blue

instance Enum Color where
  toEnum 0 = Red
  toEnum 1 = Green
  toEnum 2 = Blue
  toEnum _ = error "Invalid Color"

  fromEnum Red = 0
  fromEnum Green = 1
  fromEnum Blue = 2

nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise = succ c

mainHC7T7 :: IO ()
mainHC7T7 = do
  putStrLn "\nHC7T7 - nextColor:"
  print (nextColor Red)     -- Green
  print (nextColor Green)   -- Blue
  print (nextColor Blue)    -- Red (wrap-around)

-- HC7T8: parseShape using Read and returning Maybe
parseShape :: String -> Maybe Shape
parseShape s = readMaybe s

mainHC7T8 :: IO ()
mainHC7T8 = do
  putStrLn "\nHC7T8 - parseShape:"
  print $ parseShape "Circle 10.5"      -- Just (Circle 10.5)
  print $ parseShape "Rectangle 3 4"   -- Just (Rectangle 3.0 4.0)
  print $ parseShape "Triangle 3 4 5"  -- Nothing

-- HC7T9: Describable type class for Bool and Shape
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True = "This is True"
  describe False = "This is False"

instance Describable Shape where
  describe (Circle r) = "A circle with radius " ++ show r
  describe (Rectangle w h) = "A rectangle " ++ show w ++ " by " ++ show h

mainHC7T9 :: IO ()
mainHC7T9 = do
  putStrLn "\nHC7T9 - Describable:"
  print $ describe True
  print $ describe (Circle 4.2)
  print $ describe (Rectangle 3 5)

-- HC7T10: describeAndCompare for Describable and Ord
-- We need Ord constraint to compare, so let's require Ord also
-- But Shape and Bool don't derive Ord by default,
-- So let's restrict to types that implement Ord and Describable
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = describe $ if x >= y then x else y

-- For test, let's make Shape and Bool instances of Ord for testing:
instance Ord Color where  -- Already done above
  compare = undefined  -- keep previous instance

-- Deriving Ord for Shape (we can't automatically derive because of Double)
-- So manually:
instance Eq Shape where
  Circle r1 == Circle r2 = r1 == r2
  Rectangle w1 h1 == Rectangle w2 h2 = w1 == w2 && h1 == h2
  _ == _ = False

instance Ord Shape where
  Circle r1 <= Circle r2 = r1 <= r2
  Rectangle w1 h1 <= Rectangle w2 h2 = (w1*h1) <= (w2*h2)
  Circle _ <= Rectangle _ _ = True    -- arbitrary choice
  Rectangle _ _ <= Circle _ = False

instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False

instance Ord Bool where
  False <= True = True
  True <= False = False
  x <= y = x == y

mainHC7T10 :: IO ()
mainHC7T10 = do
  putStrLn "\nHC7T10 - describeAndCompare:"
  print $ describeAndCompare True False
  print $ describeAndCompare (Circle 3) (Circle 5)
  print $ describeAndCompare (Rectangle 4 5) (Rectangle 3 10)

-- Combined main to run all tests
main :: IO ()
main = do
  mainHC7T1
  mainHC7T2
  mainHC7T3
  mainHC7T4
  mainHC7T5
  mainHC7T6
  mainHC7T7
  mainHC7T8
  mainHC7T9
  mainHC7T10

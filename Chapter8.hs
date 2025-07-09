-- HC8T1
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr value =
  "From: " ++ fromAddr ++ ", To: " ++ toAddr ++ ", Value: " ++ show value

-- HC8T2
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data PersonPM = PersonPM
  { namePM :: String
  , addressPM :: (String, Int)
  , paymentMethod :: PaymentMethod
  } deriving Show

bob :: PersonPM
bob = PersonPM "Bob" ("123 Elm Street", 12345) Cash

-- HC8T3
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- HC8T4
data Employee = Employee
  { nameE :: String
  , experienceInYears :: Float
  } deriving Show

richard :: Employee
richard = Employee "Richard" 7.5

-- HC8T5
data Person = Person
  { name :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving Show

person1 :: Person
person1 = Person "Alice" 30 True

person2 :: Person
person2 = Person "Tom" 22 False

-- HC8T6
data ShapeR
  = CircleR { center :: (Float, Float), color :: String, radius :: Float }
  | RectangleR { width :: Float, height :: Float, color :: String }
  deriving Show

circleInstance :: ShapeR
circleInstance = CircleR (0, 0) "Red" 10.0

rectangleInstance :: ShapeR
rectangleInstance = RectangleR 5.0 10.0 "Blue"

-- HC8T7
data Animal = Dog String | Cat String

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name
describeAnimal (Cat name) = "This is a cat named " ++ name

dogInstance :: Animal
dogInstance = Dog "Buddy"

catInstance :: Animal
catInstance = Cat "Whiskers"

-- HC8T8
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, my name is " ++ n ++ " and I am " ++ show a ++ " years old."

-- HC8T9
data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr amt =
  let txId = "TX-" ++ take 6 (fromAddr ++ toAddr ++ show amt)
  in txId

-- HC8T10
data Book = Book
  { title :: String
  , author :: String
  , year :: Int
  } deriving (Show)

bookInstance :: Book
bookInstance = Book "Learn You a Haskell" "Miran Lipovača" 2011

-- Main function
main :: IO ()
main = do
  putStrLn "=== HC8T1 ==="
  putStrLn $ generateTx "Alice" "Bob" 100

  putStrLn "\n=== HC8T2 ==="
  print bob

  putStrLn "\n=== HC8T3 ==="
  putStrLn $ "Circle area: " ++ show (area (Circle 5))
  putStrLn $ "Rectangle area: " ++ show (area (Rectangle 10 5))

  putStrLn "\n=== HC8T4 ==="
  print richard

  putStrLn "\n=== HC8T5 ==="
  print person1
  print person2

  putStrLn "\n=== HC8T6 ==="
  print circleInstance
  print rectangleInstance

  putStrLn "\n=== HC8T7 ==="
  putStrLn $ describeAnimal dogInstance
  putStrLn $ describeAnimal catInstance

  putStrLn "\n=== HC8T8 ==="
  putStrLn $ greet "Eve" 25

  putStrLn "\n=== HC8T9 ==="
  putStrLn $ "Generated transaction ID: " ++ createTransaction "Addr1" "Addr2" 500

  putStrLn "\n=== HC8T10 ==="
  print bookInstance

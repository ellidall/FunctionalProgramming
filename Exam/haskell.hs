import Text.Read (readMaybe)

data Parse a = Failed String | Parsed a deriving (Show)
data NonEmpty a = NonEmpty a [a] deriving (Show)

pInt :: String -> Parse Int
pInt s = case readMaybe s of
    Just n  -> Parsed n                 
    Nothing -> Failed ("Not an integer: " ++ s)

pList :: [String] -> Parse [Int]
pList [] = Parsed []
pList (x:xs) = 
    case pInt x of
        Failed e -> Failed e
        Parsed i -> 
            case pList xs of
                Failed e -> Failed e
                Parsed is -> Parsed (i:is)

pSomeInts :: [String] -> Parse (NonEmpty Int)
pSomeInts [] = Failed "Input list is empty"
pSomeInts strs = case pList strs of
    Failed e      -> Failed e
    Parsed []     -> Failed "Input list is empty"
    Parsed (h:t)  -> Parsed (NonEmpty h t)

-- test
main :: IO ()
main = print $ pSomeInts ["123", "345", "0"]
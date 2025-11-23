data Result a
  = Success a
  | Failure String
  deriving (Show)

-- 1. Перевести строку в число (без read)
parseInt :: String -> Result Int
parseInt "" = Failure "не число (пустая строка)"
parseInt s =
  if all isDigit s
    then Success (strToInt s)
    else Failure ("не число: " ++ s)
  where
    isDigit c = c >= '0' && c <= '9'
    strToInt str = foldl (\acc c -> acc * 10 + digitToInt c) 0 str
    digitToInt c = fromEnum c - fromEnum '0'

-- 2. Перевести список строк в список чисел
parseInts :: [String] -> Result [Int]
parseInts [] = Success []
parseInts (x:xs) = case parseInt x of
  Failure msg -> Failure msg
  Success n   -> case parseInts xs of
    Failure msg -> Failure msg
    Success ns  -> Success (n : ns)

-- 3. Перевести строку в список чисел
parseListOfInt :: String -> Result [Int]
parseListOfInt s = case s of
  ('[' : inner) ->
    case reverse inner of
      (']' : _) ->
        let content = take (length inner - 1) inner
            parts = map trim (splitOn ',' content)
        in if any null parts && content /= ""
           then Failure "некорректный формат списка"
           else parseInts parts
      _ -> Failure "некорректный формат списка: ожидается [ ... ]"
  _ -> Failure "некорректный формат списка: ожидается [ ... ]"
  where
    splitOn :: Char -> String -> [String]
    splitOn _ "" = [""]
    splitOn c s = case break (== c) s of
      (xs, "") -> [xs]
      (xs, _:ys) -> xs : splitOn c ys

    trim :: String -> String
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- 4. Применить функцию к результату
mapResult :: (a -> b) -> Result a -> Result b
mapResult _ (Failure msg) = Failure msg
mapResult f (Success x)  = Success (f x)

-- 5. Применить функцию-результат к аргументу-результату
applyResult :: Result (a -> b) -> Result a -> Result b
applyResult (Failure msg) _ = Failure msg
applyResult _ (Failure msg) = Failure msg
applyResult (Success f) (Success x) = Success (f x)

main :: IO ()
main = do
  -- Задание 1: parseInt
  print (parseInt "123")
  print (parseInt "123asd")
  print (parseInt "")
  putStrLn "--------------------------------------------------------"

  -- Задание 2: parseInts
  print (parseInts ["123","345","0"])
  print (parseInts ["123","345x","0"])
  putStrLn "--------------------------------------------------------"

  -- Задание 3: parseListOfInt
  print (parseListOfInt "[123, 345, 0]")
  print (parseListOfInt "[123, 345x, 0]")
  -- Дополнительный тест: пустой список
  print (parseListOfInt "[]")
  putStrLn "--------------------------------------------------------"

  -- Задание 4: mapResult
  print (mapResult (+1) (parseInt "123"))
  print (mapResult length (parseListOfInt "[123, 345, 0]"))
  putStrLn "--------------------------------------------------------"

  -- Задание 5: applyResult
  print (applyResult (mapResult (+) (parseInt "123")) (parseInt "345"))
  -- Дополнительный тест с ошибкой
  print (applyResult (mapResult (+) (parseInt "123")) (parseInt "abc"))
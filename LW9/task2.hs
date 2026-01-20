data Mode = Debug | Verbose | Normal | Silent
  deriving (Show, Eq, Ord)

data Result a = Success a | Failure String
  deriving (Show)

data Step a = Stop | Next a
  deriving (Show)


-- 1. confirmIO :: String -> (Bool -> String) -> IO Bool
-- Запрашивает у пользователя строку и сверяет её с ожидаемым ответом.
confirmIO :: String -> (Bool -> String) -> IO Bool
confirmIO prompt renderAnswer = do
  putStrLn prompt
  input <- getLine
  let expectedYes = renderAnswer True
      expectedNo  = renderAnswer False
  if input == expectedYes
    then return True
  else if input == expectedNo
    then return False
  else do
    putStrLn "Некорректный ввод. Попробуйте снова."
    confirmIO prompt renderAnswer

-- 2. continueIO :: IO a -> IO ()
-- Выполняет действие, затем спрашивает, продолжать ли.
continueIO :: IO a -> IO ()
continueIO action = do
  _ <- action
  shouldContinue <- confirmIO "Продолжить? [да/нет]" (\b -> if b then "да" else "нет")
  when shouldContinue (continueIO action)

when :: Bool -> IO () -> IO ()
when True  action = action
when False _      = return ()

-- 3. verboseIO :: Mode -> IO () -> IO ()
-- Выполняет действие только если режим >= Verbose
verboseIO :: Mode -> IO () -> IO ()
verboseIO currentMode action =
  if currentMode <= Verbose
    then action
    else return ()

-- 4. maybeIO
maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO Nothing        = return Nothing
maybeIO (Just action)  = do
  x <- action
  return (Just x)

-- 5. sequenceResultIO
sequenceResultIO :: [IO (Result a)] -> IO [a]
sequenceResultIO actions = do
  results <- sequence actions
  return (collectSuccess results)
  where
    collectSuccess [] = []
    collectSuccess (Success x : xs) = x : collectSuccess xs
    collectSuccess (Failure _ : xs) = collectSuccess xs

-- 6. unfoldIO
unfoldIO :: (a -> IO (Step a)) -> a -> IO [a]
unfoldIO f start = do
  step <- f start
  case step of
    Stop      -> return [start]
    Next next -> do
      rest <- unfoldIO f next
      return (start : rest)

-- 7. forStateIO :: s -> [a] -> (s -> a -> IO s) -> IO s
-- Обрабатывает список слева направо, обновляя состояние.
forStateIO :: s -> [a] -> (s -> a -> IO s) -> IO s
forStateIO s []     _ = return s
forStateIO s (x:xs) f = do
  s' <- f s x        
  forStateIO s' xs f

printConsLength :: Int -> String -> IO Int
printConsLength totalLength s = do
  let n = length s
      newTotalLength = totalLength + n
  putStrLn ("добавляем " ++ show s ++ " (длина = " ++ show newTotalLength ++ ")")
  return newTotalLength  

-- 8. iforIO :: [a] -> (Int -> a -> IO b) -> IO [b]
-- Выполняет действие для каждого элемента и его индекса, собирает результаты.
iforIO :: [a] -> (Int -> a -> IO b) -> IO [b]
iforIO xs f = go 0 xs
  where
    go _ []     = return []
    go i (y:ys) = do
      b  <- f i y
      bs <- go (i + 1) ys
      return (b : bs)

example :: IO [(Int, Char)]
example = do
  results <- iforIO [1, 2] (\i n -> do
    innerResults <- iforIO "ab" (\j c -> do
      print ((i, j), replicate n c)
      return (n, c))
    return innerResults)
  return (concat results)      

main :: IO ()
main = do
  -- Тест 1
  -- b <- confirmIO "Да или нет?" (\b -> if b then "да" else "нет")
  -- print b

  -- Тест 2
  -- continueIO (putStrLn "Привет!")

  -- Тест 3
  -- verboseIO Verbose (putStrLn "Это сообщение в Verbose")
  -- verboseIO Normal  (putStrLn "Это НЕ должно появиться")

  -- Тест 4
  -- r1 <- maybeIO (Just (putStrLn "Выполняю действие..." >> return 42))
  -- print r1
  -- r2 <- maybeIO Nothing :: IO (Maybe Int)  -- ← явная аннотация
  -- print r2

  -- Тест 5
  -- result5 <- sequenceResultIO
  --   [ return (Success 10)
  --   , return (Failure "ошибка 1")
  --   , return (Success 20)
  --   , return (Failure "ошибка 2")
  --   , return (Success 30)
  --   ]
  -- print result5

  -- Тест 6
  let countdown :: Int -> IO (Step Int)
      countdown n = do
        putStrLn ("Текущее значение: " ++ show n)
        if n <= 0
          then return Stop
          else return (Next (n - 1))

  result6 <- unfoldIO countdown 3
  putStrLn ("Результат: " ++ show result6)

  let evenGen :: Int -> IO (Step Int)
      evenGen x = do
        let next = x + 2
        if next > 10
          then return Stop
          else return (Next next)

  result6b <- unfoldIO evenGen 0
  putStrLn ("Результат: " ++ show result6b)

  -- Тест 7
  -- finalLen <- forStateIO 0 ["привет", "мир", "!"] printConsLength
  -- putStrLn $ "Итоговая длина: " ++ show finalLen

  -- Тест 8
  -- result8 <- example
  -- print result8  -- Ожидается: [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
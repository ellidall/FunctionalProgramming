import Text.Read (readMaybe)
import Data.Time (Day)
import Control.Monad (ap)

data InputError
  = EmptyInput
  | CannotParse String
  | InvalidInput String
  deriving (Show)

data InputResult a
  = Success a
  | Failure InputError
  deriving (Show)

-- 1. Чтение непустой строки
textInput :: String -> InputResult String
textInput "" = Failure EmptyInput
textInput s  = Success s

-- 2. Чтение целого числа
numericInput :: String -> InputResult Int
numericInput "" = Failure EmptyInput
numericInput s = case readMaybe s of
  Just n  -> Success n
  Nothing -> Failure (CannotParse s)

-- 3. Чтение любой строки “как есть”
inputAsIs :: a -> InputResult a
inputAsIs = Success

-- 4. Чтение произвольного значения из класса Read
readInput :: Read a => String -> InputResult a
readInput "" = Failure EmptyInput
readInput s = case readMaybe s of
  Just x  -> Success x
  Nothing -> Failure (CannotParse s)

-- Упражнение 2.2

-- 1. Валидация положительного числа
newtype Positive = Positive Int deriving (Show)

positive :: Int -> Maybe Positive
positive n
  | n > 0     = Just (Positive n)
  | otherwise = Nothing

-- 2. Валидация результата с предикатом и сообщением об ошибке
validateWith :: String -> (a -> Bool) -> InputResult a -> InputResult a
validateWith _ _ (Failure err) = Failure err
validateWith msg pred (Success x)
  | pred x    = Success x
  | otherwise = Failure (InvalidInput msg)

-- 3. Валидация через Maybe-преобразователь
validateInput :: String -> (a -> Maybe b) -> InputResult a -> InputResult b
validateInput _ _ (Failure err) = Failure err
validateInput msg f (Success x) = case f x of
  Just y  -> Success y
  Nothing -> Failure (InvalidInput msg)  

-- Упражнение 2.3

-- 1. Применить функцию к результату разбора строки
mapInputResult :: (a -> b) -> InputResult a -> InputResult b
mapInputResult _ (Failure err) = Failure err
mapInputResult f (Success x)   = Success (f x)

-- 2. Совместить два результата разбора строк
combineInputResult :: InputResult (a -> b) -> InputResult a -> InputResult b
combineInputResult (Failure err) _            = Failure err
combineInputResult _            (Failure err) = Failure err
combineInputResult (Success f)  (Success x)   = Success (f x)

-- 3. Связать результат разбора с функцией-обработчиком
bindInputResult :: InputResult a -> (a -> InputResult b) -> InputResult b
bindInputResult (Failure err) _ = Failure err
bindInputResult (Success x) f   = f x  

-- Упражнение 2.4

-- Тип формы
newtype Form a = Form { runForm :: [String] -> IO (InputResult a) }

-- 1. Ввод пользователя с приглашением
prompt :: String -> IO String
prompt msg = do
  putStr msg
  getLine

-- 2. Построение "хлебных крошек"
breadcrumbs :: [String] -> String
breadcrumbs path = "[" ++ intercalate " > " path ++ "]: "
  where
    intercalate _ [] = ""
    intercalate sep (x:xs) = x ++ concatMap (sep ++) xs

-- 3. Создание формы из парсера
inputForm :: (String -> InputResult a) -> Form a
inputForm parser = Form $ \path -> do
  let msg = breadcrumbs path
  input <- prompt msg
  return (parser input)

-- Упражнение 2.5: примитивные формы

textForm :: Form String
textForm = inputForm textInput

numericForm :: Form Int
numericForm = inputForm numericInput

readForm :: Read a => Form a
readForm = inputForm readInput

-- Упражнение 2.6: повторный ввод при ошибке

-- Вспомогательная функция для красивого вывода ошибок
printInputError :: InputError -> IO ()
printInputError EmptyInput        = putStrLn "[Ошибка] Ввод не должен быть пустым (обязательное поле)."
printInputError (CannotParse s)   = putStrLn $ "[Ошибка] Непонятный ввод: " ++ s
printInputError (InvalidInput s)  = putStrLn $ "[Ошибка] Недопустимое значение: " ++ s

-- 1. Запустить форму с повторением до успеха (возвращает чистое значение a)
retryForm :: [String] -> Form a -> IO a
retryForm path form = do
  result <- runForm form path
  case result of
    Success x -> return x
    Failure e -> do
      printInputError e
      retryForm path form

-- 2. Комбинатор: обернуть форму так, чтобы она сама повторяла ввод до успеха
retry :: Form a -> Form a
retry form = Form $ \path -> do
  result <- runForm form path
  case result of
    Success x -> return (Success x)
    Failure e -> do
      printInputError e
      runForm (retry form) path

-- Упражнение 2.7: комбинаторы форм

-- 1. Сделать форму опциональной: пустой ввод → Nothing
optionalForm :: Form a -> Form (Maybe a)
optionalForm form = Form $ \path -> do
  result <- runForm form path
  case result of
    Success x          -> return (Success (Just x))
    Failure EmptyInput -> return (Success Nothing)
    Failure err        -> return (Failure err)

-- 2. Создать пустую форму (не запрашивает ввод, сразу возвращает значение)
emptyForm :: a -> Form a
emptyForm x = Form $ \_ -> return (Success x)

-- 3. Применить функцию к результату формы
mapForm :: (a -> b) -> Form a -> Form b
mapForm f (Form run) = Form $ \path -> do
  result <- run path
  return (mapInputResult f result)

-- 4. Комбинировать две формы: применить функцию из первой формы ко второй
combineForm :: Form (a -> b) -> Form a -> Form b
combineForm (Form runF) (Form runA) = Form $ \path -> do
  fRes <- runF path
  aRes <- runA path
  return (combineInputResult fRes aRes)

-- 5. Собрать n результатов с помощью одной и той же формы 
listForm :: Int -> Form a -> Form [a]
listForm n form
  | n <= 0    = emptyForm []
  | n == 1    = mapForm (: []) form
  | otherwise = combineForm (mapForm (:) form) (listForm (n - 1) form)

-- 6. Связать результат формы с другой формой
bindForm :: Form a -> (a -> Form b) -> Form b
bindForm (Form runA) k = Form $ \path -> do
  aRes <- runA path
  case aRes of
    Success x -> runForm (k x) path
    Failure e -> return (Failure e)

-- 7. Начать вложенную форму с данным названием
subform :: String -> Form a -> Form a
subform name (Form run) = Form $ \path -> run (path ++ [name])

-- 8. Начать форму с текстового описания
describe :: String -> Form a -> Form a
describe desc (Form run) = Form $ \path -> do
  putStrLn desc
  run path      

-- Упражнение 2.8: валидация форм

-- 1. Валидировать результат формы
validate :: String -> (a -> Maybe b) -> Form a -> Form b
validate msg f (Form run) = Form $ \path -> do
  result <- run path
  case result of
    Success x -> case f x of
      Just y  -> return (Success y)
      Nothing -> return (Failure (InvalidInput msg))
    Failure err -> return (Failure err)

-- 2. Валидация телефонного номера (11 цифр, начинается с 7, положительный)
newtype Phone = Phone Integer deriving (Show)

validatePhone :: Integer -> Maybe Phone
validatePhone n
  | n >= 70000000000 && n <= 79999999999 = Just (Phone n)
  | otherwise                            = Nothing

-- Форма ввода произвольного целого числа (Integer)
integerForm :: Form Integer
integerForm = inputForm $ \s ->
  case readMaybe s of
    Just n  -> Success n
    Nothing -> Failure (CannotParse s)

-- 3. Форма ввода телефонного номера
phoneForm :: Form Phone
phoneForm = validate "Некорректный ввод: Ожидается номер в международном формате (11 цифр)." validatePhone integerForm

-- 4. Валидация email (простая: должен содержать '@' и хотя бы по одному символу до и после)
newtype Email = Email String deriving (Show)

validateEmail :: String -> Maybe Email
validateEmail s =
  case break (== '@') s of
    (name, '@':domain) | not (null name) && not (null domain) -> Just (Email s)
    _                                                        -> Nothing

-- 5. Форма ввода email
emailForm :: Form Email
emailForm = validate "Некорректный ввод: Должен быть корректный адрес эл. почты." validateEmail textForm  

-- Упражнение 2.9: поля

-- 1. Обязательное текстовое поле
textField :: String -> Form String
textField name = subform name textForm

-- 2. Обязательное числовое поле (произвольный Read)
numericField :: Read a => String -> Form a
numericField name = subform name readForm

-- 3. Обязательное поле произвольного типа (Read)
readField :: Read a => String -> Form a
readField name = subform name readForm

-- 4. Произвольное поле-форма
field :: String -> Form a -> Form a
field = subform

-- Упражнение 2.10: формы для двоичных ответов

-- 1. Валидация двоичного ответа
validateBool :: String -> String -> String -> Maybe Bool
validateBool yes no input
  | input == yes = Just True
  | input == no  = Just False
  | otherwise    = Nothing

-- 2. Форма ввода с двумя вариантами ответа
boolForm :: String -> String -> Form Bool
boolForm yes no = Form $ \path -> do
  input <- prompt (breadcrumbs path)
  case validateBool yes no input of
    Just b  -> return (Success b)
    Nothing -> return (Failure (InvalidInput $ "Ответ должен быть [" ++ yes ++ "] или [" ++ no ++ "] (без скобок)."))

newtype FullName = FullName String deriving (Show)
data Role = Regular | Admin deriving (Show, Read)

data User = User
  { userName      :: FullName
  , userPhone     :: Phone
  , userBirthday  :: Maybe Day
  , userEmail     :: Maybe Email
  , userRole      :: Role
  } deriving (Show)

instance Functor Form where
  fmap = mapForm
instance Applicative Form where
  pure = emptyForm
  (<*>) = combineForm
instance Monad Form where
  (>>=) = bindForm

-- 1. Преобразование полного имени в фамилию с инициалами
shortName :: FullName -> String
shortName (FullName name) = case words name of
  []     -> ""
  (f:rs) -> f ++ concatMap (\n -> take 1 n ++ ".") (take 4 rs)  -- ограничим 5 частями

-- 2. Форма ввода данных пользователя
user :: Form User
user = do
  -- 1. Ввод ФИО
  fullName <- textField "ФИО"
  let displayName = shortName (FullName fullName)

  -- 2. Телефон (обязательный)
  phone <- subform displayName phoneForm

  -- 3. Дата рождения (опциональная)
  birthdayStr <- optionalForm (textField "Дата рождения")
  let birthday = case birthdayStr of
        Nothing -> Nothing
        Just s  -> case readMaybe s of
          Just d  -> Just d
          Nothing -> Nothing  -- Ошибка будет позже, но для простоты здесь

  -- 4. Email (опциональный)
  email <- optionalForm (field "Email" emailForm)

  -- 5. Роль (обязательная, через readField)
  role <- subform displayName (readField "Роль")

  -- Валидация даты рождения "вручную", чтобы корректно обработать ошибку
  if birthdayStr == Nothing || isJust birthday
    then return $ User (FullName fullName) phone birthday email role
    else Form $ \_ -> return (Failure (InvalidInput "Некорректная дата рождения (ожидается YYYY-MM-DD)"))

-- Вспомогательная функция (если ещё не определена)
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

-- Упражнение 2.3: Простые тесты

type Grade = Int

newtype CorrectAnswer a = CorrectIs a deriving (Show)

newtype Quiz a = Quiz { runQuiz :: Form (Grade, a) }

execQuiz :: Quiz a -> IO (Grade, a)
execQuiz q = retryForm [] (runQuiz q)

-- 1. Тест из произвольной формы с пользовательской функцией оценки
gradeWith :: (a -> Grade) -> Form a -> Quiz a
gradeWith grader form = Quiz $ do
  answer <- form
  return (grader answer, answer)

-- 2. Тест с единственным верным ответом (1 балл за правильный)
gradeCorrectAnswer :: Eq a => CorrectAnswer a -> Form a -> Quiz a
gradeCorrectAnswer (CorrectIs correct) form = Quiz $ do
  answer <- form
  let score = if answer == correct then 1 else 0
  return (score, answer)

-- 3. Вопрос с ответом да/нет
trueOrFalse :: String -> CorrectAnswer Bool -> Quiz Bool
trueOrFalse question (CorrectIs correct) = Quiz $ do
  userAnswer <- describe question (field "Ответ" (boolForm "да" "нет"))
  let score = if userAnswer == correct then 1 else 0
  return (score, userAnswer)

-- 4. Вопрос с числовым ответом
numerical :: String -> CorrectAnswer Int -> Quiz Int
numerical question (CorrectIs correct) = Quiz $ do
  userAnswer <- describe question (field "Ответ" numericForm)
  let score = if userAnswer == correct then 1 else 0
  return (score, userAnswer)

-- 5. Вопрос выбором одного варианта
singleChoice :: String -> [String] -> CorrectAnswer String -> Quiz String
singleChoice question options (CorrectIs correct) = Quiz $ do
  let optionsText = unlines $ zipWith (\i opt -> show i ++ ") " ++ opt) [1..] options
  _ <- Form $ \_ -> do
        putStrLn question
        putStr optionsText
        return (Success ())
  choiceNum <- field ("Выберите верный вариант (от 1 до " ++ show (length options) ++ ")") $
                 validate ("Ответ должен быть от 1 до " ++ show (length options))
                          (\n -> if n >= 1 && n <= length options then Just n else Nothing)
                          numericForm
  let userAnswer = options !! (choiceNum - 1)
  let score = if userAnswer == correct then 1 else 0
  return (score, userAnswer)

-- Упражнение 2.13: комбинаторы тестов

-- 1. Пустая викторина (всегда даёт 0 баллов и возвращает значение)
emptyQuiz :: a -> Quiz a
emptyQuiz x = Quiz (emptyForm (0, x))

-- 2. Применить функцию к результату теста (к значению, не к оценке)
mapQuiz :: (a -> b) -> Quiz a -> Quiz b
mapQuiz f (Quiz form) = Quiz $ do
  (grade, answer) <- form
  return (grade, f answer)

-- 3. Связывание двух викторин
bindQuiz :: Quiz a -> (a -> Quiz b) -> Quiz b
bindQuiz (Quiz formA) k = Quiz $ do
  (gradeA, answerA) <- formA
  let (Quiz formB) = k answerA
  (gradeB, answerB) <- formB
  return (gradeA + gradeB, answerB)

instance Functor Quiz where
  fmap = mapQuiz

instance Applicative Quiz where
  pure = emptyQuiz
  (<*>) = ap

instance Monad Quiz where
  (>>=) = bindQuiz  

-- Упражнение 2.14: небольшой тест о Haskell
miniQuiz :: Quiz ()
miniQuiz = do
  _ <- singleChoice
        "Какой тип у выражения \\x -> x + 1 в Haskell?"
        [ "Int -> Int"
        , "Integer -> Integer"
        , "Num a => a -> a"
        , "Float -> Float"
        ]
        (CorrectIs "Num a => a -> a")
  _ <- trueOrFalse
        "В Haskell вычисления выполняются лениво (по требованию)."
        (CorrectIs True)
  _ <- numerical
        "Сколько элементов в списке [1..3] ++ [4,5]?"
        (CorrectIs 5)
  return ()

main :: IO ()
main = do
  putStrLn "=== Задача 1 ==="
  print $ textInput ""
  print $ textInput "Hello"

  putStrLn "\n=== Задача 2 ==="
  print $ numericInput "123"
  print $ numericInput ""
  print $ numericInput "1.5"

  putStrLn "\n=== Задача 3 ==="
  print $ inputAsIs "Hello"

  putStrLn "\n=== Задача 4 ==="
  print (readInput "(123,False)" :: InputResult (Int, Bool))
  print (readInput "" :: InputResult (Int, Bool))
  print (readInput "(15," :: InputResult (Int, Bool))

  putStrLn "\n=== Упр. 2.2: positive ==="
  print $ positive 123
  print $ positive (-123)
  print $ positive 0

  putStrLn "\n=== Упр. 2.2: validateWith ==="
  print $ validateWith "Число должно быть больше 0." (> 0) (numericInput "-123")
  print $ validateWith "Число должно быть больше 0." (> 0) (numericInput "123")
  print $ validateWith "Число должно быть больше 0." (> 0) (numericInput "1.23")
  print $ validateWith "Число должно быть больше 0." (> 0) (numericInput "")

  putStrLn "\n=== Упр. 2.2: validateInput ==="
  print $ validateInput "Число должно быть положительным." positive (readInput "-123")
  print $ validateInput "Число должно быть положительным." positive (readInput "123")
  print $ validateInput "Число должно быть положительным." positive (readInput "1.23")
  print $ validateInput "Число должно быть положительным." positive (readInput "")

  putStrLn "\n=== Упр. 2.3: mapInputResult ==="
  print $ mapInputResult length (textInput "hello")
  print $ mapInputResult (+10) (numericInput "5")
  print $ mapInputResult length (textInput "")

  putStrLn "\n=== Упр. 2.3: combineInputResult ==="
  print $ combineInputResult (mapInputResult (+) (numericInput "2")) (numericInput "3")
  print $ combineInputResult (mapInputResult (+) (numericInput "")) (numericInput "3")
  print $ combineInputResult (mapInputResult (+) (numericInput "2")) (numericInput "abc")

  putStrLn "\n=== Упр. 2.3: bindInputResult ==="
  let f (xs :: [Int]) = readInput (concatMap show xs) :: InputResult Int
  print $ bindInputResult (readInput "[1, 2, 3]") f
  print $ bindInputResult (readInput "") f
  print $ bindInputResult (readInput "[1, 2, 3") f

  -- putStrLn "\n=== Упр. 2.4: prompt ==="
  -- name <- prompt "What is your name? "
  -- putStrLn $ "Hello, " ++ name ++ "!"

  -- putStrLn "\n=== Упр. 2.4: breadcrumbs ==="
  -- print $ breadcrumbs ["one","two","three"]
  -- print $ breadcrumbs []

  -- putStrLn "\n=== Упр. 2.4: inputForm ==="
  -- result1 <- runForm (inputForm numericInput) ["age"]
  -- print result1
  -- result2 <- runForm (inputForm textInput) ["user", "name"]
  -- print result2

  -- putStrLn "\n=== Упр. 2.5: textForm ==="
  -- resultText <- runForm textForm ["user", "name"]
  -- print resultText

  -- putStrLn "\n=== Упр. 2.5: numericForm ==="
  -- resultNum <- runForm numericForm ["user", "age"]
  -- print resultNum

  -- putStrLn "\n=== Упр. 2.5: readForm (Double) ==="
  -- resultDouble <- runForm (readForm :: Form Double) ["user", "height"]
  -- print resultDouble

  -- putStrLn "\n=== Упр. 2.6: retryForm ==="
  -- age <- retryForm ["user", "age"] numericForm
  -- putStrLn $ "Ваш возраст: " ++ show age

  -- putStrLn "\n=== Упр. 2.6: retry ==="
  -- resultRetry <- runForm (retry numericForm) ["input"]
  -- print resultRetry

  -- putStrLn "\n=== Упр. 2.7: optionalForm ==="
  -- resultOpt1 <- runForm (optionalForm textForm) ["name"]
  -- print resultOpt1
  -- resultOpt2 <- runForm (optionalForm numericForm) ["age"]
  -- print resultOpt2

  -- putStrLn "\n=== Упр. 2.7: emptyForm ==="
  -- resultEmpty <- runForm (emptyForm 42) ["ignored"]
  -- print resultEmpty

  -- putStrLn "\n=== Упр. 2.7: mapForm ==="
  -- resultMap <- runForm (mapForm length textForm) ["any text"]
  -- print resultMap

  -- putStrLn "\n=== Упр. 2.7: combineForm ==="
  -- resultCombine <- runForm (combineForm (mapForm (+) numericForm) numericForm) ["int"]
  -- print resultCombine

  -- putStrLn "\n=== Упр. 2.7: listForm ==="
  -- resultList <- runForm (listForm 2 numericForm) ["item"]
  -- print resultList

  -- putStrLn "\n=== Упр. 2.7: bindForm ==="
  -- resultBind <- runForm (bindForm numericForm (\n -> listForm n numericForm)) ["count"]
  -- print resultBind

  -- putStrLn "\n=== Упр. 2.7: subform ==="
  -- resultSub <- runForm (subform "details" (subform "age" numericForm)) ["user"]
  -- print resultSub

  -- putStrLn "\n=== Упр. 2.7: describe ==="
  -- resultDesc <- runForm (describe "Сколько будет 2 + 2?" numericForm) ["answer"]
  -- print resultDesc  

  -- putStrLn "\n=== Упр. 2.8: validate (через positive) ==="
  -- resultVal <- runForm (validate "Число должно быть положительным." positive numericForm) ["int"]
  -- print resultVal

  -- putStrLn "\n=== Упр. 2.8: validatePhone ==="
  -- print $ validatePhone 79991234567
  -- print $ validatePhone 1234567
  -- print $ validatePhone (-1234567890)

  -- putStrLn "\n=== Упр. 2.8: phoneForm (с retryForm) ==="
  -- phone <- retryForm ["тел."] phoneForm
  -- putStrLn $ "Телефон: " ++ case phone of Phone p -> show p

  -- putStrLn "\n=== Упр. 2.8: validateEmail ==="
  -- print $ validateEmail "someone@ispring.com"
  -- print $ validateEmail "best_email"

  -- putStrLn "\n=== Упр. 2.8: emailForm (с retryForm) ==="
  -- email <- retryForm ["email"] emailForm
  -- putStrLn $ "Email: " ++ case email of Email e -> show e

  -- putStrLn "\n=== Упр. 2.9: textField ==="
  -- resultTf <- runForm (textField "name") ["user"]
  -- print resultTf

  -- putStrLn "\n=== Упр. 2.9: numericField ==="
  -- resultNf <- runForm (numericField "age" :: Form Int) ["user"]
  -- print resultNf

  -- putStrLn "\n=== Упр. 2.9: readField (Double) ==="
  -- resultRf <- runForm (readField "height" :: Form Double) ["user"]
  -- print resultRf

  -- putStrLn "\n=== Упр. 2.9: field ==="
  -- resultFld <- runForm (field "тел." phoneForm) []
  -- print resultFld

  -- putStrLn "\n=== Упр. 2.10: validateBool ==="
  -- print $ validateBool "да" "нет" "да"
  -- print $ validateBool "да" "нет" "наверное"

  -- putStrLn "\n=== Упр. 2.10: boolForm ==="
  -- resultBool1 <- runForm (boolForm "да" "нет") []
  -- print resultBool1
  -- resultBool2 <- runForm (boolForm "yes" "no") ["confirm"]
  -- print resultBool2

  -- putStrLn "\n=== Упр. 2.11: shortName ==="
  -- putStrLn $ shortName (FullName "Иванов Иван Иванович")
  -- putStrLn $ shortName (FullName "Склодовская-Кюри Мария")
  -- let longName = "Длинныйчулок Пеппилотта Виктуалия Рульгардина Крисминта Эфраимсдоттер"
  -- putStrLn $ shortName (FullName longName)

  -- putStrLn "\n=== Упр. 2.11: user form ==="
  -- userResult <- runForm user []
  -- print userResult

  -- putStrLn "\n=== Упр. 2.12: gradeWith ==="
  -- resultQuiz1 <- runForm (runQuiz (gradeWith (\x -> if x == 4 then 100 else 0) (field "2 + 2 =" numericForm))) []
  -- print resultQuiz1

  -- putStrLn "\n=== Упр. 2.12: gradeCorrectAnswer ==="
  -- let form2 = describe "2 + 2 = ?" (numericField "Ответ")
  -- resultQuiz2 <- runForm (runQuiz (gradeCorrectAnswer (CorrectIs 4) form2)) []
  -- print resultQuiz2

  -- putStrLn "\n=== Упр. 2.12: trueOrFalse (запуск через execQuiz) ==="
  -- Этот тест интерактивный, лучше запускать отдельно, но можно так:
  -- (закомментируй, если не хочешь, чтобы main зависал)
  -- quiz3 <- execQuiz (trueOrFalse "2 + 2 = 4?" (CorrectIs True))
  -- print quiz3

  -- putStrLn "\n=== Упр. 2.12: numerical (через execQuiz) ==="
  -- quiz4 <- execQuiz (numerical "2 + 2 = ?" (CorrectIs 4))
  -- print quiz4

  -- putStrLn "\n=== Упр. 2.12: singleChoice (через execQuiz) ==="
  -- quiz5 <- execQuiz (singleChoice "2 + 2 = ?" ["3", "4", "5", "не знаю"] (CorrectIs "4"))
  -- print quiz5

  -- putStrLn "\n=== Упр. 2.13: emptyQuiz ==="
  -- resultEmptyQ <- execQuiz (emptyQuiz "готово")
  -- print resultEmptyQ

  -- putStrLn "\n=== Упр. 2.13: mapQuiz ==="
  -- resultMapQ <- execQuiz (mapQuiz show (numerical "2 + 2 = ?" (CorrectIs 4)))
  -- print resultMapQ

  -- putStrLn "\n=== Упр. 2.13: bindQuiz ==="
  -- let q1 = trueOrFalse "Отвечать будете?" (CorrectIs True)
  --     q2 True  = numerical "2 + 2 = ?" (CorrectIs 4)
  --     q2 False = emptyQuiz 0
  -- resultBindQ <- execQuiz (bindQuiz q1 q2)
  -- print resultBindQ

  putStrLn "\n=== Упр. 2.14: miniQuiz ==="
  (totalScore, _) <- execQuiz miniQuiz
  putStrLn $ "Ваш результат: " ++ show totalScore ++ " / 3"
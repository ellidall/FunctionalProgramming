-- Исходные определения
twice f x = f (f x)
dup   f x = f x x

-- Общие типы (выведенные):
-- twice :: (a -> a) -> a -> a
-- dup   :: (a -> a -> b) -> a -> b

-- 1. twice (+1) 0
--    (+1) :: Num a => a -> a, 0 :: Num a => a
--    Инстанцирование: a = Integer
--    Результат: 2

-- 2. twice (++ "!") "Hello"
--    (++ "!") :: String -> String, "Hello" :: String
--    Инстанцирование: a = String
--    Результат: "Hello!!"

-- 3. dup (+) 123
--    (+) :: Num a => a -> a -> a, 123 :: Num a => a
--    Инстанцирование: a = b = Integer
--    Результат: 246

-- 4. dup (dup (++)) "Hello"
--    (++) :: [c] -> [c] -> [c]
--    Внутренний dup (++) :: [c] -> [c]
--    Внешний dup применяет его дважды → эквивалентно (x ++ x) ++ (x ++ x)
--    Инстанцирование: a = [Char], b = [Char]
--    Результат: "HelloHelloHelloHello"

-- 5. twice (dup (.) (+1)) 0
--    (.) :: (b -> c) -> (a -> b) -> a -> c
--    dup (.) (+1) = (+1) . (+1) = \x -> x + 2
--    Инстанцирование: a, b (dup) = Integer -> Integer
--    Инстанцирование: a (twice) = Integer

-- 6. twice dup
--    twice :: (α -> α) -> α -> α, dup :: (a -> a -> b) -> a -> b
--    α -> α = (a -> a -> b) -> a -> b |=> α = a -> (a -> b) = a -> b
--    α -> α = (a -> a -> b) -> a -> b |=> α = a -> (a
--    Ответ ошибка, тк b = a -> b

-- 7. twice twice (+1) 0
--    twice twice (+1) = twice (twice (+1)) = twice (+2) = (+4)
--    Инстанцирование:
--      • Внутренний twice: a = Integer
--      • Внешний twice: a = Integer -> Integer
--    Результат: 4

-- 8. twice twice twice
--    Частичное применение: ((twice twice) twice)
--    Внутренний twice: a1 | (a1 -> a1) -> a1 -> a1
--    Средний twice: a2 = a1 -> a1 | (a1 -> a1) -> a1 -> a1 = a2 -> a2
--    Внешний twice: a3 = (a1 -> a1) -> (a1 -> a1)
-- Упражнение 8.2 (Подсчёт возможных реализаций)

-- Задача 1
-- a)
-- f1 :: Int -> Int
-- Бесконечно много реализаций (любая функция Int -> Int).
-- Примеры: id, (+1), const 0, abs, и т.д.

-- b)
-- f2 :: a -> a
-- Только одна реализация:
f2 :: a -> a
f2 x = x  -- это id

-- c)
-- f3 :: a -> Int
-- Любая реализация должна игнорировать аргумент и возвращать константу.
-- Реализаций столько, сколько значений Int (бесконечно много).
-- Пример:
f3 :: a -> Int
f3 _ = 42  -- или любая другая константа

-- d)
-- f4 :: Int -> a
-- Нет реализаций: невозможно создать значение произвольного типа a.
-- Любая попытка приведёт к ошибке или undefined (partial function).

-- Задача 2
-- a)
-- g1 :: (a, b) -> a
-- Только одна реализация:
g1 :: (a, b) -> a
g1 (x, _) = x  -- fst

-- b)
-- g2 :: (a, b) -> b
-- Только одна реализация:
g2 :: (a, b) -> b
g2 (_, y) = y  -- snd

-- c)
-- g3 :: (a, a) -> a
-- Две реализации: взять левый или правый элемент.
g3_left, g3_right :: (a, a) -> a
g3_left  (x, _) = x
g3_right (_, y) = y

-- Задача 3
-- a)
-- h1 :: Bool -> Bool
-- Четыре total реализации:
h1_id, h1_not, h1_true, h1_false :: Bool -> Bool
h1_id     x = x
h1_not    x = not x
h1_true   _ = True
h1_false  _ = False

-- b)
-- h2 :: Maybe a -> Bool
-- Четыре реализации: зависит только от конструктора (Nothing/Just).
h2_alwaysFalse, h2_alwaysTrue, h2_isJust, h2_isNothing :: Maybe a -> Bool
h2_alwaysFalse _          = False
h2_alwaysTrue  _          = True
h2_isJust      (Just _)   = True
h2_isJust      Nothing    = False
h2_isNothing   (Just _)   = False
h2_isNothing   Nothing    = True

-- c)
-- h3 :: Maybe a -> Maybe a
-- Две total реализации:
h3_id, h3_nothing :: Maybe a -> Maybe a
h3_id      m = m
h3_nothing _ = Nothing

-- Задача 4
-- a)
-- k1 :: (a -> b) -> a -> b
-- Только одна реализация:
k1 :: (a -> b) -> a -> b
k1 f x = f x  -- это ($)

-- b)
-- k2 :: (a -> a) -> a -> a
-- Бесконечно много реализаций: n-кратное применение f (n = 0,1,2,...)
k2_0, k2_1, k2_2 :: (a -> a) -> a -> a
k2_0 _ x = x                -- 0 раз
k2_1 f x = f x              -- 1 раз
k2_2 f x = f (f x)          -- 2 раза
-- и так далее...

-- c)
-- k3 :: (a -> a) -> a -> [a]
-- Бесконечно много реализаций: списки вида [x, f x, f (f x), ...] конечной длины
k3_0, k3_1, k3_2 :: (a -> a) -> a -> [a]
k3_0 _ x = []
k3_1 f x = [x]
k3_2 f x = [x, f x]
-- и т.д.

-- Задача 5
-- a)
-- t1 :: (a -> b) -> [a] -> [b]
-- Только одна "естественная" total реализация — map:
t1 :: (a -> b) -> [a] -> [b]
t1 f xs = map f xs

-- b)
-- t2 :: (a -> Bool) -> [a] -> [a]
-- Бесконечно много реализаций, но основные:
t2_id, t2_empty, t2_filter, t2_filterNot :: (a -> Bool) -> [a] -> [a]
t2_id        _ xs = xs                     -- ничего не фильтровать
t2_empty     _ _  = []                     -- всегда пустой
t2_filter    p xs = filter
newtype Day = Day Int deriving (Show)
newtype Month = Month Int deriving (Show)
newtype Year = Year Int deriving (Show)
data Date = Date Day Month Year deriving (Show)

today :: Date
today = Date (Day 13) (Month 10) (Year 2025)

-- №1
yearOf :: Date -> Year
yearOf (Date _ _ y) = y

monthOf :: Date -> Month
monthOf (Date _ m _) = m

dayOf :: Date -> Day
dayOf (Date d _ _) = d

-- №2
prettyDate :: Date -> String
prettyDate (Date (Day d) (Month m) (Year y)) =
  show d ++ " " ++ monthName m ++ " " ++ show y
  where
    monthName 1  = "января"
    monthName 2  = "февраля"
    monthName 3  = "марта"
    monthName 4  = "апреля"
    monthName 5  = "мая"
    monthName 6  = "июня"
    monthName 7  = "июля"
    monthName 8  = "августа"
    monthName 9  = "сентября"
    monthName 10 = "октября"
    monthName 11 = "ноября"
    monthName 12 = "декабря"
    monthName _  = "unknown"

-- №3
isLeapYear :: Year -> Bool
isLeapYear (Year y) =
  (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)

-- №4
daysInMonth :: Year -> Month -> Int
daysInMonth y (Month m) =
  case m of
    1  -> 31
    2  -> if isLeapYear y then 29 else 28
    3  -> 31
    4  -> 30
    5  -> 31
    6  -> 30
    7  -> 31
    8  -> 31
    9  -> 30
    10 -> 31
    11 -> 30
    12 -> 31
    _  -> 0

-- №5
addYears :: Int -> Date -> Date
addYears n (Date d m (Year y)) = Date d m (Year (y + n))

addMonths :: Int -> Date -> Date
addMonths n (Date (Day d) (Month m) (Year y)) =
  let totalMonths = m + n - 1
      newYear = y + totalMonths `div` 12
      newMonth = totalMonths `mod` 12 + 1 
      newDay = min d (daysInMonth (Year newYear) (Month newMonth))
  in Date (Day newDay) (Month newMonth) (Year newYear)

addDays :: Int -> Date -> Date
addDays 0 date = date
addDays n date@(Date (Day d) (Month m) (Year y))
  | n < 0 = error "addDays: negative days not supported"
  | d < daysInMonth (Year y) (Month m) =
      addDays (n - 1) (Date (Day (d + 1)) (Month m) (Year y))
  | m < 12 =
      addDays (n - 1) (Date (Day 1) (Month (m + 1)) (Year y))
  | otherwise =
      addDays (n - 1) (Date (Day 1) (Month 1) (Year (y + 1)))

main :: IO ()
main = do
  putStrLn "========== 1. Извлечение компонентов даты =========="
  print $ yearOf today
  print $ monthOf today
  print $ dayOf today

  putStrLn "========== 2. Красивый вывод даты =========="
  putStrLn $ prettyDate today

  putStrLn "========== 3. Високосный год =========="
  print $ isLeapYear (yearOf today)
  print $ isLeapYear (Year 2024)
  print $ isLeapYear (Year 2000)
  print $ isLeapYear (Year 1900)

  putStrLn "========== 4. Дней в месяце =========="
  print $ daysInMonth (Year 2000) (Month 2)  -- 29
  print $ daysInMonth (Year 2025) (Month 2)  -- 28
  print $ daysInMonth (Year 2025) (Month 4)  -- 30

  putStrLn "========== 5. Добавление лет, месяцев, дней =========="
  putStrLn $ prettyDate (addYears 1 today)
  putStrLn $ prettyDate (addMonths 3 today)
  putStrLn $ prettyDate (addDays 10 today)
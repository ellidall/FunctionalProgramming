import Data.Char (toUpper, isLetter)

-- 1. Простое эхо: читает построчно и печатает как есть
echo :: IO ()
echo = do
  line <- getLine
  putStrLn line
  echo 

-- 2. Эхо с переводом в верхний регистр
shoutEcho :: IO ()
shoutEcho = do
  line <- getLine
  putStrLn (map toUpper line)
  shoutEcho

-- 3. Эхо по словам: каждое слово -> предложение (с точкой), только буквы
echoByWord :: IO ()
echoByWord = do
  line <- getLine
  putStrLn (wordsToSentences line)
  echoByWord

wordsToSentences :: String -> String
wordsToSentences s = unwords sentencesWithDots
  where
    lettersOnly = map (\c -> if isLetter c then c else ' ') s
    words' = filter (not . null) (words lettersOnly)
    sentencesWithDots = map (++ ".") words'

main :: IO ()
main = echoByWord
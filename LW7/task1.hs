-- №1
secondToLast :: [a] -> Maybe a
secondToLast [] = Nothing
secondToLast [_] = Nothing
secondToLast [x, _] = Just x
secondToLast (_ : xs) = secondToLast xs

-- №2
myReverse :: [a] -> [a]
myReverse xs = helper [] xs
  where
    helper acc [] = acc
    helper acc (x : xs) = helper (x : acc) xs

-- №3
myFlatten :: [[a]] -> [a]
myFlatten []       = []
myFlatten (xs:xss) = xs ++ myFlatten xss

-- №4
myEncode :: (Eq a) => [a] -> [(a, Int)]
myEncode [] = []
myEncode (x : xs) = encodeAcc xs x 1
  where
    encodeAcc [] c n = [(c, n)]
    encodeAcc (y : ys) c n
      | y == c = encodeAcc ys c (n + 1)
      | otherwise = (c, n) : encodeAcc ys y 1

-- №5
myDecode :: [(a, Int)] -> [a]
myDecode [] = []
myDecode ((x, n) : xs) = replicate x n ++ myDecode xs
  where
    replicate _ 0 = []
    replicate v k = v : replicate v (k - 1)

-- №6
mySplit :: Int -> [a] -> ([a], [a])
mySplit n xs
  | n <= 0 = ([], xs)
  | otherwise = splitAcc n xs []
  where
    splitAcc _ [] acc = (reverse acc, [])
    splitAcc 0 ys acc = (reverse acc, ys)
    splitAcc k (y : ys) acc = splitAcc (k - 1) ys (y : acc)


-- №7
chunks :: Int -> [a] -> [[a]]
chunks n xs
  | n <= 0 = []
  | otherwise = chunkAcc n xs
  where
    chunkAcc _ [] = []
    chunkAcc k ys =
      let (chunk, rest) = takeN k ys
       in chunk : chunkAcc k rest

    takeN 0 ys = ([], ys)
    takeN _ [] = ([], [])
    takeN m (z : zs) =
      let (cs, rs) = takeN (m - 1) zs
       in (z : cs, rs)

-- №8
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x : xs)
  | n < 0 = []
  | otherwise = prepend x (choose (n - 1) xs) ++ choose n xs
  where
    prepend _ [] = []
    prepend v (ys : yss) = (v : ys) : prepend v yss

main :: IO ()
main = do
  putStrLn "========== 1. secondToLast =========="
  print $ secondToLast "abcd"
  print $ secondToLast [1, 2, 3]
  print $ secondToLast [1]

  putStrLn "========== 2. myReverse =========="
  print $ myReverse [1, 2, 3]
  print $ myReverse "abcd"
  print $ myReverse ([] :: [Int])

  putStrLn "========== 3. myFlatten =========="
  print $ myFlatten [["a"], ["b", "c"], ["d"]]

  putStrLn "========== 4. myEncode =========="
  print $ myEncode "aaaabccaadeeee"

  putStrLn "========== 5. myDecode =========="
  print $ myDecode [('a', 4), ('b', 1), ('c', 2), ('a', 2), ('d', 1), ('e', 4)]

  putStrLn "========== 6. mySplit =========="
  print $ mySplit 3 "abcdefghik"

  putStrLn "========== 7. chunks =========="
  print $ chunks 4 "abcdefghik"

  putStrLn "========== 8. choose =========="
  print $ choose 3 "abcde"

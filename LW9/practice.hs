newtype Item = Item {getItem :: String}

newtype TodoState = TodoState {getTodoState :: [Item]}

mysequence_ :: [IO ()] -> IO ()
mysequence_ [] = return ()
mysequence_ (program : programs) = do
    program
    mysequence_ programs

printItems :: [Item] -> IO ()
printItems [] = putStrLn "Нет дел"
printItems items = mysequence_ (map (putStrLn . getItem) items)

main :: IO ()
main = do
  let myTodo =
        TodoState
          { getTodoState =
              [ Item {getItem = "Зарядка"},
                Item {getItem = "Книга"},
                Item {getItem = "Работа"},
                Item {getItem = "Haskell"}
              ]
          }
  putStrLn "Мой список дел:"
  printItems (getTodoState myTodo)
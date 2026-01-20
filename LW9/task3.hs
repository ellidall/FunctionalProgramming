import Text.Read (readMaybe)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Directory (doesFileExist)

newtype Item = Item { getItem :: String }
  deriving (Show, Read, Eq)

data TodoState = TodoState
  { currentItems :: [Item]
  , history      :: [[Item]]
  }
  deriving (Show)

data Request
  = Exit
  | ListItems
  | AddItem Item
  | RemoveItem Int
  | SaveFile
  | LoadFile
  | Undo
  deriving (Show)

data Response = Response String (Maybe TodoState)
  deriving (Show)

type Handler = TodoState -> Response

todoFileName :: FilePath
todoFileName = "todo.dat"

deleteAt :: Int -> [a] -> Maybe (a, [a])
deleteAt _ [] = Nothing
deleteAt 0 (x:xs) = Just (x, xs)
deleteAt n (x:xs)
  | n > 0 = case deleteAt (n - 1) xs of
      Nothing       -> Nothing
      Just (y, ys)  -> Just (y, x : ys)
  | otherwise = Nothing


saveToFile :: [Item] -> IO ()
saveToFile items = writeFile todoFileName (unlines (map getItem items))

loadFromFile :: IO [Item]
loadFromFile = do
  exists <- doesFileExist todoFileName
  if exists
    then do
      content <- readFile todoFileName
      return (map Item (lines content))
    else return []

parseRequest :: String -> Maybe Request
parseRequest input = case words input of
  ["выход"]       -> Just Exit
  ["дела"]        -> Just ListItems
  ("сделано":ws)  -> case ws of
    [strIndex] -> case readMaybe strIndex of
      Just n  -> Just (RemoveItem n)
      Nothing -> Nothing
    _ -> Nothing
  ["сохранить"]   -> Just SaveFile
  ["загрузить"]   -> Just LoadFile
  ["отмена"]      -> Just Undo
  _ -> Just (AddItem (Item input))

withHistory :: [Item] -> TodoState -> TodoState
withHistory newItems state = state
  { currentItems = newItems
  , history = currentItems state : history state
  }

handleListItems :: Handler
handleListItems state = Response msg (Just state)
  where
    items = currentItems state
    msg = if null items
      then "Нет дел (всё сделано)."
      else unlines (map getItem items)

handleAddItem :: Item -> Handler
handleAddItem item state = Response msg (Just newState)
  where
    msg = "Добавлено дело: " ++ getItem item
    newState = withHistory (item : currentItems state) state

handleRemoveItem :: Int -> Handler
handleRemoveItem i state = case deleteAt i (currentItems state) of
  Just (Item name, rest) ->
    Response ("Дело выполнено: " ++ name) (Just (withHistory rest state))
  Nothing ->
    Response ("Дела номер " ++ show i ++ " не существует") (Just state)

handleSaveFile :: Handler
handleSaveFile state = Response "Сохранено в файл." (Just state)

handleLoadFile :: Handler
handleLoadFile state = Response "Загружено из файла." (Just newState)
  where
    newState = TodoState (currentItems state) []

handleUndo :: Handler
handleUndo state = case history state of
  []      -> Response "Нет действий для отмены." (Just state)
  (prev:rest) -> Response "Отменено." (Just (state { currentItems = prev, history = rest }))

handleExit :: Handler
handleExit _ = Response "Пока!" Nothing

handleRequest :: Request -> Handler
handleRequest Exit         = handleExit
handleRequest ListItems    = handleListItems
handleRequest (AddItem i)  = handleAddItem i
handleRequest (RemoveItem i) = handleRemoveItem i
handleRequest SaveFile     = handleSaveFile
handleRequest LoadFile     = handleLoadFile
handleRequest Undo         = handleUndo

processIO :: Request -> TodoState -> IO TodoState
processIO SaveFile state = do
  saveToFile (currentItems state)
  return state
processIO LoadFile state = do
  items <- loadFromFile
  return (TodoState items [])
processIO _ state = return state

todoApp :: TodoState -> IO ()
todoApp state = do
  putStrLn "Введите запрос:"
  input <- getLine
  case parseRequest input of
    Nothing -> do
      putStrLn "Не получилось понять ваш запрос :("
      todoApp state
    Just request -> do
      let response = handleRequest request state
      case response of
        Response msg Nothing -> do
          putStrLn msg
          saveToFile (currentItems state)
        Response msg (Just newState) -> do
          putStrLn msg
          newState' <- processIO request newState
          todoApp newState'

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  initialItems <- loadFromFile
  let initialState = TodoState initialItems []
  todoApp initialState
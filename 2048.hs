import Data.List
import System.IO
import System.Random

data Tile = Blank | Number Int deriving (Eq, Show)

type Board = [[Tile]]

showBoard :: Board -> String
showBoard rows = unlines $ interpose hline (map showRow rows)
  where
    n = length (head rows)
    hline = intervene "+" (replicate n "----")

showRow :: [Tile] -> String
showRow tiles =
  intervene "|" (replicate n "    ") ++ "\n" ++
  intervene "|" (map showTile tiles) ++ "\n" ++
  intervene "|" (replicate n "    ")
  where
    n = length tiles

    showTile Blank = "    "
    showTile (Number k) = pad 4 (show k)

interpose :: a -> [a] -> [a]
interpose sep xs = [sep] ++ intersperse sep xs ++ [sep]

intervene :: [a] -> [[a]] -> [a]
intervene sep = concat . interpose sep

pad :: Int -> String -> String
pad padding string =
  if n < padding then
    replicate (padding - n) ' ' ++ string
  else
    string
  where
    n = length string

slideLeft :: Board -> Board
slideLeft rows = map slideRowLeft rows

slideRowLeft :: [Tile] -> [Tile]
slideRowLeft tiles = take n $ mergeTiles nonBlankTiles ++ repeat Blank
  where
    n = length tiles
    nonBlankTiles = [tile | tile@(Number _) <- tiles]

    mergeTiles (Number k : Number l : restTiles) | k == l =
      Number (k + l) : mergeTiles restTiles
    mergeTiles (Number k : restTiles) = Number k : mergeTiles restTiles
    mergeTiles [] = []

slideRight :: Board -> Board
slideRight = reflect . slideLeft . reflect
  where
    reflect = map reverse

slideUp :: Board -> Board
slideUp = transpose . slideLeft . transpose

slideDown :: Board -> Board
slideDown = transpose . slideRight . transpose

addRandomTile :: Board -> IO Board
addRandomTile board = do
  (rowIndex, colIndex) <- choice $ blankTilesPositions board
  tile <- choice [Number 2, Number 4]
  return $ addTile tile rowIndex colIndex board

choice :: [a] -> IO a
choice xs = do
  i <- randomRIO (0, n-1)
  return $ xs !! i
  where
    n = length xs

blankTilesPositions :: Board -> [(Int, Int)]
blankTilesPositions board =
  [ (rowIndex, colIndex)
  | (rowIndex, row) <- zip [0..] board
  , (colIndex, Blank) <- zip [0..] row
  ]

addTile :: Tile -> Int -> Int -> Board -> Board
addTile tile rowIndex colIndex =
  modifyNth rowIndex (modifyNth colIndex (const tile))

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth n f xs = xs' ++ [f x] ++ xs''
  where
    (xs', x : xs'') = splitAt n xs

emptyBoard :: Board
emptyBoard = replicate 4 (replicate 4 Blank)

initialBoard :: IO Board
initialBoard = addRandomTile =<< addRandomTile emptyBoard

displayBoard :: Board -> IO ()
displayBoard board = do
  cls
  writeAt (0, 0) (showBoard board)

cls :: IO ()
cls = putStr "\ESC[2J"

writeAt :: (Int, Int) -> String -> IO ()
writeAt p xs = do
  goto p
  putStr xs

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  loop =<< initialBoard

loop :: Board -> IO ()
loop board = do
  displayBoard board
  case () of
    _ | isComplete board -> putStrLn "You win!"
      | isStuck board -> putStrLn "Game over!"
      | otherwise -> loop =<< step board

step :: Board -> IO Board
step board = do
  key <- getChar
  let board' = slide key board
  if board' == board then step board else addRandomTile board'

isComplete :: Board -> Bool
isComplete = any (any (== Number 2048))

isStuck :: Board -> Bool
isStuck board =
  all (== board) $ map ($ board)
    [ slideUp
    , slideDown
    , slideLeft
    , slideRight
    ]

slide :: Char -> Board -> Board
slide key = case key of
  'w' -> slideUp
  's' -> slideDown
  'a' -> slideLeft
  'd' -> slideRight
  _   -> id

import Control.Monad.IO.Class
import Data.List
import System.Random
import Text.Printf
import UI.NCurses

data Tile = Blank | Number Int deriving (Eq, Show)

type Board = [[Tile]]

showBoard :: Board -> String
showBoard rows = unlines $ interpose hline (map showRow rows)
  where
    n = length (head rows)
    hline = intervene "+" (replicate n "------")

showRow :: [Tile] -> String
showRow tiles =
  intervene "|" (replicate n "      ") ++ "\n" ++
  intervene "|" (map showTile tiles)   ++ "\n" ++
  intervene "|" (replicate n "      ")
  where
    n = length tiles

    showTile Blank = "      "
    showTile (Number k) = printf " %4d " k

interpose :: a -> [a] -> [a]
interpose sep xs = [sep] ++ intersperse sep xs ++ [sep]

intervene :: [a] -> [[a]] -> [a]
intervene sep = concat . interpose sep

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

main :: IO ()
main = runCurses $ do
  setEcho False
  play =<< defaultWindow

play :: Window -> Curses ()
play window = loop window =<< liftIO initialBoard

loop :: Window -> Board -> Curses ()
loop window board = do
  displayBoard board
  case () of
    _ | isComplete board ->
          endGameOrReplay "You win! Try again (y/n)? "
      | isStuck board ->
          endGameOrReplay "Game over! Try again (y/n)? "
      | otherwise ->
          loop window =<< step board
  where
    displayBoard board = do
      updateWindow window $ do
        moveCursor 0 0
        drawString $ showBoard board
      render

    endGameOrReplay status = do
      displayStatus status
      tryAgain <- getResponse
      if tryAgain
        then clearStatus status >> play window
        else return ()

    displayStatus status = do
      updateWindow window $ do
        moveCursor 17 0
        drawString status
      render

    getResponse = do
      event <- getEvent window Nothing
      case event of
        Just (EventCharacter 'y') -> return True
        Just (EventCharacter 'n') -> return False
        _ -> getResponse

    clearStatus status = do
      updateWindow window $ do
        moveCursor 17 0
        drawString $ replicate (length status) ' '
        moveCursor 17 0
      render

    step board = do
      event <- getEvent window Nothing
      let board' = slide event board
      if board' == board
        then step board
        else liftIO $ addRandomTile board'

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

slide :: Maybe Event -> Board -> Board
slide event = case event of
  Just (EventSpecialKey KeyUpArrow) -> slideUp
  Just (EventSpecialKey KeyDownArrow) -> slideDown
  Just (EventSpecialKey KeyLeftArrow) -> slideLeft
  Just (EventSpecialKey KeyRightArrow) -> slideRight
  _ -> id

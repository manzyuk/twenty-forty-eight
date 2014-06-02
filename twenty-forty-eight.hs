import Control.Monad.IO.Class
import Data.List
import System.Random
import Text.Printf
import UI.NCurses

data Tile = Blank | Number Int deriving Eq

type Board = [[Tile]]

data GameState = GameState
  { gameBoard :: Board
  , gameScore :: Int
  }
  deriving Eq

showGameState :: GameState -> String
showGameState (GameState board score) =
  showBoard board ++ printf "Score: %4d\n" score

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

slideLeft :: GameState -> GameState
slideLeft (GameState board score) = GameState board' (score + sum scores)
  where
    (board', scores) = unzip $ map slideRowLeft board

slideRowLeft :: [Tile] -> ([Tile], Int)
slideRowLeft tiles = (take n $ mergedNonBlankTiles ++ repeat Blank, score)
  where
    n = length tiles
    nonBlankTiles = [tile | tile@(Number _) <- tiles]

    (mergedNonBlankTiles, score) = mergeTiles nonBlankTiles

    mergeTiles = go [] 0
      where
        go result points (Number k : Number l : restTiles) | k == l =
          go (Number (k + l) : result) (points + k + l) restTiles
        go result points (Number k : restTiles) =
          go (Number k : result) points restTiles
        go result points [] = (reverse result, points)

slideRight :: GameState -> GameState
slideRight = reflectBoard . slideLeft . reflectBoard

reflectBoard :: GameState -> GameState
reflectBoard (GameState board score) = GameState (map reverse board) score

slideUp :: GameState -> GameState
slideUp = transposeBoard . slideLeft . transposeBoard

transposeBoard :: GameState -> GameState
transposeBoard (GameState board score) = GameState (transpose board) score

slideDown :: GameState -> GameState
slideDown = transposeBoard . slideRight . transposeBoard

addRandomTile :: GameState -> IO GameState
addRandomTile (GameState board score) = do
  (rowIndex, colIndex) <- choice $ blankTilesPositions board
  tile <- choice [Number 2, Number 4]
  return $ GameState (addTile tile rowIndex colIndex board) score

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

initialGameState :: IO GameState
initialGameState = addRandomTile =<< addRandomTile (GameState emptyBoard 0)

main :: IO ()
main = runCurses $ do
  setEcho False
  play =<< defaultWindow

play :: Window -> Curses ()
play window = loop window =<< liftIO initialGameState

loop :: Window -> GameState -> Curses ()
loop window state@(GameState board _) = do
  displayGameState state
  case () of
    _ | isComplete board ->
          endGameOrReplay "You win! Try again (y/n)? "
      | isStuck state ->
          endGameOrReplay "Game over! Try again (y/n)? "
      | otherwise ->
          loop window =<< step state
  where
    displayGameState state = do
      updateWindow window $ do
        moveCursor 0 0
        drawString $ showGameState state
      render

    endGameOrReplay status = do
      displayStatus status
      tryAgain <- getResponse
      if tryAgain
        then clearStatus status >> play window
        else return ()

    displayStatus status = do
      updateWindow window $ do
        moveCursor 18 0
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
        moveCursor 18 0
        drawString $ replicate (length status) ' '
        moveCursor 18 0
      render

    step state = do
      event <- getEvent window Nothing
      let state' = slide event state
      if state' == state
        then step state
        else liftIO $ addRandomTile state'

isComplete :: Board -> Bool
isComplete = any (any (== Number 2048))

isStuck :: GameState -> Bool
isStuck state =
  all (== state) $ map ($ state)
    [ slideUp
    , slideDown
    , slideLeft
    , slideRight
    ]

slide :: Maybe Event -> GameState -> GameState
slide event = case event of
  Just (EventSpecialKey KeyUpArrow) -> slideUp
  Just (EventSpecialKey KeyDownArrow) -> slideDown
  Just (EventSpecialKey KeyLeftArrow) -> slideLeft
  Just (EventSpecialKey KeyRightArrow) -> slideRight
  _ -> id

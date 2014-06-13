import Data.List
import Data.Char
import System.IO
import Control.Monad (when)

main :: IO ()
main = do
  h <- openFile "sudoku.txt" ReadMode
  puzzleFileStr <- hGetContents h
  let puzzles = parsePuzzleStr puzzleFileStr
  putStrLn $ "Parsed " ++ show (length puzzles) ++ " puzzles!"

  sequence_ $ map (\ p ->
                    do
                      putStrLn "Orig puzzle:"
                      (putStrLn . puzzleToStr)  p
                      let filled = fillObvious p
                      let solved = 0 `notElem` filled
                      case solved of True -> do
                                       putStrLn "Solution:"
                                       (putStrLn . puzzleToStr) p
                                     False -> do
                                       let recSolved = recSolve filled
                                       case recSolved of Nothing -> putStrLn "Could not solve it!"
                                                         Just p -> do
                                                           putStrLn "Solution:"
                                                           (putStrLn . puzzleToStr) p
                  )
    puzzles

puzzleToStr :: [Int] -> String
puzzleToStr puzzle = concat [s | row <- [0..8], col <- [0..8],
                             let idx = col + (row * 9),
                             let v = puzzle !! idx,
                             let sep = if col == 8 then "\n" else " ",
                             let s = show v ++ sep]

everyN :: Int -> [a] -> [[a]]
everyN _ [] = []
everyN 0 _ = []
everyN n xs = [take n xs] ++ everyN n (drop n xs)

parsePuzzleStr :: String -> [[Int]]
parsePuzzleStr s = let withoutR = filter (/= '\r') s -- haskell doesn't drop the /r in line seperators
                       ps = concat $ filter (not . isPrefixOf "Grid") $ lines withoutR
                   in map (map digitToInt) $ everyN 81 ps

-- Takes index, returns an array of indexes in the same subgrid
subGridIdxs :: Int -> [Int]
subGridIdxs idx = let col = idx `mod` 9
                      row = idx `div` 9
                      scol = col `div` 3 * 3
                      srow = row `div` 3 * 3
                  in [scol + c + (srow + r) * 9  | r <- [0..2], c <- [0..2]]

columnValues :: [Int] -> Int -> [Int]
columnValues puzzle idx = let col = idx `mod` 9
                          in [av| r <- [0..8], let aidx = col + (r * 9), let av = puzzle !! aidx, av /= 0]

rowValues :: [Int] -> Int -> [Int]
rowValues puzzle idx = let row = idx `div` 9
                       in [av | c <- [0..8], let aidx = c + (row * 9), let av = puzzle !! aidx, av /= 0]

subGridValues :: [Int] -> Int -> [Int]
subGridValues puzzle idx = filter (\ v -> v /= 0) $ map (\ aidx -> puzzle !! aidx) $ subGridIdxs idx

possByIdx :: [Int] -> Int -> [Int]
possByIdx puzzle idx
  | v == 0 = let colVs = columnValues puzzle idx
                 rowVs = rowValues puzzle idx
                 subGridVs = subGridValues puzzle idx
             in [1..9] \\ (colVs ++ rowVs ++ subGridVs) 
  | otherwise = [v]
  where v = puzzle !! idx

fillObvious :: [Int] -> [Int]
fillObvious puzzle =
  let filled = [case v of 0 -> let possibilities = possByIdx puzzle idx
                               in if (length possibilities) == 1 then possibilities !! 0 else 0
                          _ -> v
                          | idx <- [0..80], let v = puzzle !! idx]
  in if filled == puzzle then filled else fillObvious filled

isSolved :: [Int] -> Bool
isSolved puzzle = 0 `notElem` puzzle

orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y

firstM :: [Maybe a] -> Maybe a
firstM [] = Nothing
firstM (x:xs) = case x of
                  Just _ -> x
                  Nothing -> firstM xs

recSolve :: [Int] -> Maybe [Int]
recSolve puzzle =
  let unfilledIdxs = filter (\ i -> puzzle !! i == 0) [0..80]
      allPoss = map (\ i -> (possByIdx puzzle i, i)) unfilledIdxs
      -- Find the idx with the smallest number of possibilities
      (poss, idx) = minimumBy (\ (ps1, _) (ps2, _)  -> compare (length ps1) (length ps2)) allPoss
  in case poss of [] -> Nothing   -- There is a square with 0 possibilities. Cannot solve.
                  _  -> let p_puzzles = map (\ p -> [if i == idx then p else puzzle !! i | i <- [0..80]]) poss
                            p_puzzles_f = map fillObvious p_puzzles
                            solution = (find isSolved p_puzzles) `orElse` (find isSolved p_puzzles_f)
                        in solution `orElse` firstM (map recSolve p_puzzles_f)

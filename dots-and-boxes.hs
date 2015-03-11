--- PRACTICA 2 
--- Guido Arnau Antoniucci

import System.Random

data Board = Board (Int,Int) [(Line,Bool)] Score Bool
-- the tuple indicates the size of the board
-- the list are the lines that have been drawn in the 
-- board ordered by increasing time in which were drawn
-- Bool indicates player 1 turn (true) or player 2 turn

instance Show Board where
  show b@(Board (w,1) _ sc _) = showRow b ++ "\n\nScore is: "  ++ show sc ++ "\n"
  show b@(Board (w,h) lines sc tu) = showRow b ++ "\n" ++ showColums b ++ "\n" ++ show (Board (w,h-1) lines sc tu)

showRow :: Board -> String
showRow b@(Board (w,h) lines _ _) = concatMap (\n -> if ((n-1,h-1),(n,h-1)) `elem` map fst lines then (if (((n-1,h-1),(n,h-1)),True) `elem` lines then "+###" else "+@@@") else "+   " ) [1..w-1] ++ "+"

showColums :: Board -> String
showColums b@(Board (w,h) lines _ _) = concatMap (\n -> if ((n-1,h-2),(n-1,h-1)) `elem` map fst lines then (if (((n-1,h-2),(n-1,h-1)),True) `elem` lines then "#   " else "@   ") else "    " ) [1..w-1] 
                                        ++ (if ((w-1,h-2),(w-1,h-1)) `elem` map fst lines then (if (((w-1,h-2),(w-1,h-1)),True) `elem` lines then "#" else "@") else " " ) 

type Line = (Coord,Coord)
type Coord = (Int,Int)
type Score = (Int,Int)
type Strategy = Board -> IO Board

-- it is considered that the coordinate (0,0)
-- is at the bottom left side of the grid
-- the coord of the top right corner is (w-1,h-1)

pickRandElem :: [a] -> IO a
pickRandElem xs = do r <- getStdRandom (randomR (1, length xs))
                     return (xs !! (r-1))

areAdjacent :: Coord -> Coord -> Bool
areAdjacent (a,b) (c,d) = (abs (c - a) == 1 && abs (d - b) == 0) || (abs (c - a) == 0 && abs (d - b) == 1)

isLineNotOccupied :: Board -> Line -> Bool
isLineNotOccupied b@(Board d lines _ _) (c1,c2) = (c1,c2) `notElem` map fst lines && (c2,c1) `notElem` map fst lines

allPossibleMoves :: Board -> [Line]
allPossibleMoves bo@(Board (w,h) _ _ _) = foldr (\(c1,c2) z -> if (c2,c1) `elem` z then z else (c1,c2):z) [] (filter (isLineNotOccupied bo) [(a,b) | a <- all , b <- all, areAdjacent a b])
                                          where all = [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)]]

------------------RANDOM STRATEGY  
getRandomLine :: Board -> IO Line
getRandomLine b@(Board (w,h) _ _ _) = do x1 <- randomRIO (0,w-1)
                                         y1 <- randomRIO (0,h-1)
                                         x2 <- randomRIO (0,w-1)
                                         y2 <- randomRIO (0,h-1)
                                         if areAdjacent (x1,y1) (x2,y2) && isLineNotOccupied b ((x1,y1),(x2,y2)) then 
                                            return ((x1, y1), (x2, y2))
                                         else getRandomLine b

randomStrat :: Strategy
randomStrat b@(Board _ _ _ tu) = do --l <- pickRandElem (allPossibleMoves b)
                                   l <- getRandomLine b
                                   putStrLn (if tu then "Player 1 moves to:" else "Player 2 moves to:")
                                   print (normalizeLine l)
                                   return (applyMove b (normalizeLine l))

------------------INTELLIGENT STRATEGY
isThirdLineH :: Board -> Line -> Bool
isThirdLineH (Board _ lines _ _) ((x1,y1),(x2,y2)) 
    | (b1 && b2) || (b2 && b3) || (b3 && b1) || (b4 && b5) || (b5 && b6) || (b6 && b4) = True
    | otherwise = False
    where b1 = normalizeLine ((x1,y1),(x1, y1-1)) `elem` map fst lines
          b2 = normalizeLine ((x1,y1-1),(x1+1, y1-1)) `elem` map fst lines
          b3 = normalizeLine ((x1+1,y1-1),(x2, y2)) `elem` map fst lines
          b4 = normalizeLine ((x1,y1),(x1, y1+1)) `elem` map fst lines
          b5 = normalizeLine ((x1,y1+1),(x1+1, y1+1)) `elem` map fst lines
          b6 = normalizeLine ((x1+1,y1+1),(x2, y2)) `elem` map fst lines

isThirdLineV :: Board -> Line -> Bool
isThirdLineV (Board _ lines _ _) ((x1,y1),(x2,y2)) 
    | (b1 && b2) || (b2 && b3) || (b3 && b1) || (b4 && b5) || (b5 && b6) || (b6 && b4) = True
    | otherwise = False
    where b1 = normalizeLine ((x1,y1),(x1-1, y1)) `elem` map fst lines
          b2 = normalizeLine ((x1-1,y1),(x1-1, y1+1)) `elem` map fst lines
          b3 = normalizeLine ((x1-1,y1+1),(x2, y2)) `elem` map fst lines
          b4 = normalizeLine ((x1,y1),(x1+1, y1)) `elem` map fst lines
          b5 = normalizeLine ((x1+1,y1),(x1+1, y1+1)) `elem` map fst lines
          b6 = normalizeLine ((x1+1,y1+1),(x2, y2)) `elem` map fst lines

leastScoreOpponent :: Board -> [Line] -> IO Line
leastScoreOpponent b apm = pickRandElem (if null tl then apm else tl) --allPossibleMoves (apm) will never be empty
                           where tl = filter ((\m -> not (if isMoveHor m then isThirdLineH b m else isThirdLineV b m)) . normalizeLine) apm

getScoringLine :: Board -> IO Line
getScoringLine b = if null asm then leastScoreOpponent b apm
                   else pickRandElem asm
                   where asm = filter ((> 0) . isSquareMove b . normalizeLine) apm
                         apm = allPossibleMoves b

intelligentStrat :: Strategy
intelligentStrat b@(Board _ _ _ tu) = do putStrLn (if tu then "Player 1 moves to:" else "Player 2 moves to:")
                                         l <- getScoringLine b
                                         print (normalizeLine l)
                                         return (applyMove b (normalizeLine l))

normalizeLine :: Line -> Line
normalizeLine (c1,c2) = if c1 < c2 then (c1,c2)
                        else (c2,c1)

------------------ EASY STRATEGY

getEasyLine :: Board -> IO Line
getEasyLine b = pickRandElem (if null asm then apm else asm)
                where asm = filter ((> 0) . isSquareMove b . normalizeLine) apm
                      apm = allPossibleMoves b

easyStrat :: Strategy
easyStrat b@(Board _ _ _ tu) = do putStrLn (if tu then "Player 1 moves to:" else "Player 2 moves to:")
                                  l <- getEasyLine b
                                  print (normalizeLine l)
                                  return (applyMove b (normalizeLine l))

------------------INPUT STRATEGY 
isCoordInside :: Board -> Coord -> Bool
isCoordInside (Board (w,h) _ _ _) (x,y) = x < w && x >= 0 && y < h && y >= 0


playerStrat :: Strategy
playerStrat b = do putStrLn "\n"
                   print b
                   putStrLn "# -> Player lines, @ -> AI lines"
                   putStrLn "Coordinates to draw a VALID line to, in format ( hor x y )  or ( ver x y ):"
                   l <- getLine
                   if not $ isLineNotOccupied b (normalizeLine (inputToLine l)) then do putStrLn "Line already drawn"
                                                                                        playerStrat b
                   else do putStrLn "Player 1 moves to:"
                           print (normalizeLine (inputToLine l))
                           return (applyMove b (normalizeLine (inputToLine l)))

------------------ GAME LOGIC
isGameDone :: Board -> Bool
isGameDone (Board (w,h) l _ _) = length l == 2*w*h - h - w 

isMoveHor :: Line -> Bool
isMoveHor l = abs (fst (fst l) - fst (snd l)) == 1

isMoveVer :: Line -> Bool
isMoveVer = not . isMoveHor

isSquareMoveH :: Board -> Line -> Int
isSquareMoveH (Board _ lines _ _) ((x1,y1),(x2,y2)) 
    | b1 && b2 = 2
    | b1 || b2 = 1
    | otherwise = 0
    where b1 = normalizeLine ((x1,y1),(x1, y1-1)) `elem` map fst lines &&
               normalizeLine ((x1,y1-1),(x1+1, y1-1)) `elem` map fst lines &&
               normalizeLine ((x1+1,y1-1),(x2, y2)) `elem` map fst lines
          b2 = normalizeLine ((x1,y1),(x1, y1+1)) `elem` map fst lines &&
               normalizeLine ((x1,y1+1),(x1+1, y1+1)) `elem` map fst lines &&
               normalizeLine ((x1+1,y1+1),(x2, y2)) `elem` map fst lines

isSquareMoveV :: Board -> Line -> Int
isSquareMoveV (Board _ lines _ _) ((x1,y1),(x2,y2)) 
    | b1 && b2 = 2
    | b1 || b2 = 1
    | otherwise = 0
    where b1 = normalizeLine ((x1,y1),(x1-1, y1)) `elem` map fst lines &&
               normalizeLine ((x1-1,y1),(x1-1, y1+1)) `elem` map fst lines &&
               normalizeLine ((x1-1,y1+1),(x2, y2)) `elem` map fst lines
          b2 = normalizeLine ((x1,y1),(x1+1, y1)) `elem` map fst lines &&
               normalizeLine ((x1+1,y1),(x1+1, y1+1)) `elem` map fst lines &&
               normalizeLine ((x1+1,y1+1),(x2, y2)) `elem` map fst lines

isSquareMove :: Board -> Line -> Int
isSquareMove b l = if isMoveHor l then isSquareMoveH b l
                   else isSquareMoveV b l

-- returns how many squares will be created by that line

updateScore :: Score -> Int -> Bool -> Score
updateScore (s1,s2) p tu = if tu then (s1+p, s2)
                           else (s1, s2+p)

-- sets the score to the correct player

insert :: Ord a => (a -> a -> Bool) -> [a] -> a -> [a]
insert p [] n = [n]
insert p (x:xs) n
  | p x n = x : insert p xs n
  | otherwise = n:x:xs

applyMove :: Board -> Line -> Board
applyMove b@(Board d lines sc tu) l = Board d ((l,tu):lines) (updateScore sc p tu) (if p > 0 then tu else not tu)
                                    where p = isSquareMove b l

-- a line should always be represented with the lowest coordinate in the
-- left side of the tuple (random generates tuples from lower to higher, not a problem)
-- it is assumed:
-- 1. the line is not already occupied 
-- 2. the coordinates of the move are inside the grid bounds
-- 3. the two coordinates of the line are adjecent

-- it applies the line to the board and updates the score accordingly,
-- also sets the turn to the other player

doGame :: Board -> Strategy -> Strategy -> IO ()
doGame b@(Board _ _ _ t) e1 e2
    | isGameDone b = printFinalScore b
    | otherwise = do newboard <- if t then e1 b else e2 b
                     doGame newboard e1 e2

---- FIRST INPUT HANDLING 
getAI :: String -> Strategy
getAI "1" = randomStrat
getAI "2" = easyStrat
getAI "3" = intelligentStrat

getSizeBoard :: String -> (Int,Int)
getSizeBoard s = (read (head w), read (last w))
                where w = words s

printFinalScore :: Board -> IO ()
printFinalScore b@(Board _ _ sc _) = do --print b
                                        putStrLn "\nGAME FINISHED!"
                                        putStrLn "\nFinal score:"
                                        print sc

inputToLine :: String -> Line
inputToLine s = if head w == "hor" then ((read (w !! 1), read (w !! 2)),(read (w !! 1) + 1, read (w !! 2)))
                else ((read (w !! 1), read (w !! 2)),(read (w !! 1), read (w !! 2) +1))
                where w = words s


main = do putStrLn "DOTS AND BOXES"
          putStrLn "Input number of dots in format 'width height':"
          s <- getLine
          putStrLn "Select: 1. Player vs AI"
          putStrLn "        2. AI vs AI"
          m <- getLine
          if read m == 1 then do putStrLn "Select AI: 1.Random"
                                 putStrLn "           2.Easy"
                                 putStrLn "           3.Intelligent"
                                 ai <- getLine
                                 putStrLn "Who goes first? 1.Player 2.AI"
                                 t <- getLine
                                 putStrLn "REMEMBER: the (0,0) coordinate is at the left bottom corner of the table."
                                 putStrLn "          Also, the first value of the coordinate indicates the width and the"
                                 putStrLn "          second value indicates the height."
                                 if read t == 1 then doGame (Board (getSizeBoard s) [] (0,0) True) playerStrat (getAI ai)
                                 else doGame (Board (getSizeBoard s) [] (0,0) False) playerStrat (getAI ai)
          else do putStrLn "Select AI nº1: 1.Random"
                  putStrLn "               2.Easy"
                  putStrLn "               3.Intelligent"
                  putStrLn "\nThe selected AI will go first."
                  ai1 <- getLine
                  putStrLn "Select AI nº2: 1.Random"
                  putStrLn "               2.Easy"
                  putStrLn "               3.Intelligent"
                  ai2 <- getLine
                  doGame (Board (getSizeBoard s) [] (0,0) True) (getAI ai1) (getAI ai2)

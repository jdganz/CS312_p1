module ConnectFour where

import Data.Function (on)
import Data.List (sortBy)
import Data.List (sortOn)

-- To run it, try:
-- ghci
-- :load ConnectFour

data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

              -- values passed: running wins, type of game, player turn
data Result = EndOfGame Double [Int] String Int        -- end of game w/ winner
            | ContinueGame State [Int] String Int      -- main state w/ state
            | StartGame [Int] String Int               -- start of game
            | ChoosePlayer [Int] String Int            -- choose player
         deriving (Eq, Show)

type Game = Action -> State -> [Int] -> String -> Int -> Result

type Player = State -> Action

------ Connect Four Game -------

type Action = Int
type InternalState = ([[Int]], Int)   -- a list of columns and their values in the slots, and an Int saying who's turn it is

connect4 :: Game
connect4 move (State board@(rawboard, rawturn) available) totals playerType playerTurn -- move is the column dropped into
    | win (mydrop move board) (getdroprow (rawboard !! move)) move      = EndOfGame (fromIntegral rawturn) 
                                                                          (if (fromIntegral rawturn) == 1 
                                                                          then [(totals !! 0)+1, (totals !! 1)]
                                                                          else [(totals !! 0), (totals !! 1)+1])
                                                                          playerType playerTurn
    | length available == 1 && head (tail (rawboard !! move)) /= 0      = EndOfGame 0 totals playerType playerTurn       -- no more moves, tie
    | otherwise                                                         =
          ContinueGame (State (mydrop move board)
                        (getactions (mydrop move board))) totals playerType playerTurn

-------- CHECKING WIN --------------------
-- win board (InternalState) take in a board state and the row col coordinate of the last made move and checks if the player who just went (the one who is not currently playing) has won
win :: InternalState -> Int -> Int -> Bool
win (board, whosturn) droprow dropcol
    | (checkRight board droprow dropcol (if whosturn == 1 then 2 else 1)) + (checkLeft board droprow dropcol (if whosturn == 1 then 2 else 1)) >= 5 = True
    | (checkUp board droprow dropcol (if whosturn == 1 then 2 else 1)) + (checkDown board droprow dropcol (if whosturn == 1 then 2 else 1)) >= 5 = True
    | (checkDiagRightUp board droprow dropcol (if whosturn == 1 then 2 else 1)) + (checkDiagLeftDown board droprow dropcol (if whosturn == 1 then 2 else 1)) >= 5 = True
    | (checkDiagRightDown board droprow dropcol (if whosturn == 1 then 2 else 1)) + (checkDiagLeftUp board droprow dropcol (if whosturn == 1 then 2 else 1)) >= 5 = True
    | otherwise = False

-- function to check to the right of a given coordinate in the board and count how many of the right type there are
checkRight :: [[Int]] -> Int -> Int -> Int -> Int
checkRight board row col match
    | col == 6 = if ((board !! col) !! row) == match then 1 else 0
    | ((board !! col) !! row) == match = 1 + (checkRight board row (col + 1) match)
    | otherwise = 0

-- function to check to the left of a given coordinate in the board and count how many of the right type there are
checkLeft :: [[Int]] -> Int -> Int -> Int -> Int
checkLeft board row col match
    | col == 0 = if ((board !! col) !! row) == match then 1 else 0
    | ((board !! col) !! row) == match = 1 + (checkLeft board row (col - 1) match)
    | otherwise = 0    

-- function to check to up of a given coordinate in the board and count how many of the right type there are
checkUp :: [[Int]] -> Int -> Int -> Int -> Int
checkUp board row col match
    | row == 0 = if ((board !! col) !! row) == match then 1 else 0
    | ((board !! col) !! row) == match = 1 + (checkUp board (row - 1) col match)
    | otherwise = 0    

-- function to check to down of a given coordinate in the board and count how many of the right type there are
checkDown :: [[Int]] -> Int -> Int -> Int -> Int
checkDown board row col match
    | row == 5 = if ((board !! col) !! row) == match then 1 else 0
    | ((board !! col) !! row) == match = 1 + (checkDown board (row + 1) col match)
    | otherwise = 0    

-- function to check to diagonally to the right and upwards of a given coordinate in the board and count how many of the right type there are
checkDiagRightUp :: [[Int]] -> Int -> Int -> Int -> Int
checkDiagRightUp board row col match
    | row == 0 || col == 6 = if ((board !! col) !! row) == match then 1 else 0
    | ((board !! col) !! row) == match = 1 + (checkDiagRightUp board (row - 1) (col + 1) match)
    | otherwise = 0
    
-- function to check to diagonally to the left and downwards of a given coordinate in the board and count how many of the right type there are
checkDiagLeftDown :: [[Int]] -> Int -> Int -> Int -> Int
checkDiagLeftDown board row col match
    | row == 5 || col == 0 = if ((board !! col) !! row) == match then 1 else 0
    | ((board !! col) !! row) == match = 1 + (checkDiagLeftDown board (row + 1) (col - 1) match)
    | otherwise = 0    
    
-- function to check to diagonally to the right and downwards of a given coordinate in the board and count how many of the right type there are
checkDiagRightDown :: [[Int]] -> Int -> Int -> Int -> Int
checkDiagRightDown board row col match
    | row == 5 || col == 6 = if ((board !! col) !! row) == match then 1 else 0
    | ((board !! col) !! row) == match = 1 + (checkDiagRightDown board (row + 1) (col + 1) match)
    | otherwise = 0    

-- function to check to diagonally to the left and upwards of a given coordinate in the board and count how many of the right type there are
checkDiagLeftUp :: [[Int]] -> Int -> Int -> Int -> Int
checkDiagLeftUp board row col match
    | row == 0 || col == 0 = if ((board !! col) !! row) == match then 1 else 0
    | ((board !! col) !! row) == match = 1 + (checkDiagLeftUp board (row - 1) (col - 1) match)
    | otherwise = 0    

---------------------------------


-- function to get the row that the piece drops into
getdroprow :: [Int] -> Int
getdroprow [] = -1
getdroprow boardcol@(h:t)
    | head boardcol /= 0 = -1
    | otherwise = 1 + getdroprow t

-- mydrop function takes an action (which is just an Int), and a board (an InternalState) and returns the new board (InternalState)
-- mydrop by mapping the last non zero element of the board at column move to turn
mydrop :: Action -> InternalState -> InternalState
mydrop move (board, turn)
    | move == 0 = ([(dropincol 0 turn (board !! move))]++(tail board), if turn == 1 then 2 else 1)
    | move == 6 = ((take move board) ++ [(dropincol 0 turn (board !! move))], if turn == 1 then 2 else 1)
    | otherwise = ((take move board)++[(dropincol 0 turn (board !! move))]++(takeRemaining move board), if turn == 1 then 2 else 1)

-- given a list of integers, change the last matching element to newgiven
dropincol :: Int -> Int -> [Int] -> [Int]
dropincol match newgiven col@(h:t)
    | not (elem match t) = newgiven:t
    | otherwise = h:(dropincol match newgiven t)

-- given an Integer and a list of Integers return the list of elements after the Int
takeRemaining :: Int -> [a] -> [a]
takeRemaining after (h:t)
    | after == 0 = t
    | otherwise = takeRemaining (after - 1) t
    
-- get available moves from a board state (the columns that are not full)
getactions :: InternalState -> [Action]
getactions (board, whosturn) = filter (\ a -> a /= 99) (foldl (\ a b -> if head b == 0 then a++[length a] else a++[99]) [] board)
   
connect4_start = State ([[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]],1) [0..6]

connect4_start1 = State ([[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]],1) [0..6]

connect4_start2 = State ([[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]],2) [0..6]

------- A Player -------

-- a simple player that always chooses the first column that isn't full
simple_player :: Player
simple_player (State _ avail) = head avail


-- easyAI:
-- AI checks if they will win on any next possible move -> if so places there
-- If not checks opponents next possible moves, and if they would win by placing token in a specific column -> AI places in that column
-- Else places in first available column
-- TODO: change to random column OR change to middle out OR change to most connections but w/o the 2 turn check like medium ai
easyAI :: Player
easyAI (State board@(rawboard, rawturn) available)
        | myWin         = myCol
        | theirWin      = theirCol
        | otherwise     = 
            let nextMoves = reverse (sortOn snd (findNextMoves2 (State (rawboard, rawturn) available)))
            in 
            let (nextCol, numAdj) = head nextMoves
            in
                 if  foldl (\acc x -> acc && x) True (map (\(a,b) -> (b == numAdj)) (tail nextMoves))
                 then available !! ((length available) `div` 2)
                 else nextCol           -- 
            where 
                (myCol, myWin)          = checkAnyPlayerWin (State (rawboard, rawturn) available)           -- check if I win for any move
                playerTurn              = if (rawturn == 1) then 2 else 1
                (theirCol, theirWin)    = checkAnyPlayerWin (State (rawboard, playerTurn) available)        -- check if they win for any move


-- checkAnyPlayerWin state:
-- Takes in a State and returns an (Int x, Bool b) tuple where
    -- Int x = the row in which token would win the game    
    -- Bool b = True if Player would win next turn by placing at x
    --      or False if there is no win
    -- (7,False) returned when no win is found (since it checked each column)
-- recurses over list of available actions to determine if one of them would result in a win:

checkAnyPlayerWin :: State -> (Int, Bool)

-- List of available actions is empty -> must not have found a win -> return (7, FALSE)
checkAnyPlayerWin (State board@(rawboard, rawturn) []) = (7, False)

checkAnyPlayerWin (State board@(rawboard, rawturn) (h:t))
    | win (mydrop h board) (getdroprow (rawboard !! h)) h   = (h, True)
    | otherwise                                             = checkAnyPlayerWin (State (rawboard, rawturn) t)

-- Col > 6 -> must not have found a win -> return (7, FALSE)
-- checkAnyPlayerWin 7 (State board@(rawboard, rawturn) available) = (7, False)



-- MEDIUM AI Player: (2 move check ahead)
-- Checks if theres a winning move for itself -> if so return that move
-- else checks if theres a winning move for the player -> if so then return that move to block players win
-- if no winning move on next turn:
--  - Find list of best moves for computer (from left to right: position at which token placement would have the most consecutive tokens)
--      - check to see if first move in this list doesnt result in a win for player on the next turn:
--          - if not: return this move
--          - if so: check next move
mediumAI :: Player
mediumAI (State board@(rawboard, rawturn) available) 
    | myWinNextMove         = myWinNextCol
    | theirWinNext          = theirWinNextCol
    | otherwise             = 
        let nextMoves = reverse (sortOn snd (findNextMoves2 (State (rawboard, rawturn) available))) 
        in findNextBestMove (State (rawboard, rawturn) available) nextMoves
                                        
        where 
            (myWinNextCol, myWinNextMove)           = checkAnyPlayerWin (State (rawboard, rawturn) available)           -- check if I win for any move
            playerTurn                              = if (rawturn == 1) then 2 else 1
            (theirWinNextCol, theirWinNext)         = checkAnyPlayerWin (State (rawboard, playerTurn) available)        -- check if they win for any move





-- findNextBestMove board nextmoves:
-- Given sorted list of (col, numAdj): returns first col such that the next move (opponents) does not result in a win:
-- if it does then check the next item in the list:
findNextBestMove :: State -> [(Int, Int)] -> Int

findNextBestMove (State board@(rawboard, rawturn) available) [] = head available

findNextBestMove (State board@(rawboard, rawturn) available) ((col, numAdj): t) = 
    let  playerTurn = if (rawturn == 1) then 2 else 1 in
     let newBoard = mydrop col board
     in
     let (theirWinNextCol, theirWinNext) = checkAnyPlayerWin (State newBoard (getactions newBoard))  -- drop piece into board at col. (need to getactions because just dropped a token)
     in
     if theirWinNext then findNextBestMove (State board available) t    
     else 
        if  foldl (\acc x -> acc && x) True (map (\(a,b) -> b == numAdj) t)     -- if all numAdj values are the same, then place in the middle col of available cols
            then available !! ((length available) `div` 2)
            else col             -- TODO check here if all are equal. if so -> then return the middle one



-- findNextMoves state:
-- Returns a list of (Int col,Int adj) pairs, 
--      where col is the column in which token would be placed, and
--            adj is the number of adj tokens of the same player
findNextMoves :: State -> [(Int,Int)]
findNextMoves (State board@(rawboard, rawturn) []) = []
findNextMoves (State board@(rawboard, rawturn) (h:t)) =
    let dropRow = (getdroprow (rawboard !! h)) in
    (h, (maxNumAdjacent rawboard (if dropRow < 5 then (dropRow + 1) else dropRow) h rawturn)) : findNextMoves (State (rawboard, rawturn) t)


-- findNextMoves2 state:
-- Returns a list of (Int col,Int adj) pairs, 
--      where col is the column in which token would be placed, and
--            adj is the number of adj tokens of the same player
findNextMoves2 :: State -> [(Int,Int)]
findNextMoves2 (State board@(rawboard, rawturn) []) = []
findNextMoves2 (State board@(rawboard, rawturn) (h:t)) =
    let dropRow = (getdroprow (rawboard !! h)) in
    let newboard = (mydrop h board) in
    (h, (maxNumAdjacent (getBoardAsList newboard) dropRow h rawturn)) : findNextMoves2 (State (rawboard, rawturn) t)



-- getBoardList board:
getBoardAsList :: InternalState -> [[Int]]
getBoardAsList board@(rawboard, rawturn) = rawboard

-- totalNumAdjacent board row col match:
-- Return the  number of adjacent tokens of that matching token (in all directions), given a board and a row and column
totalNumAdjacent :: [[Int]] -> Int -> Int -> Int -> Int
totalNumAdjacent board row col match =
    let 
        right           = checkRight board row col match
        left            = checkLeft board row col match
        up              = checkUp board row col match
        down            = checkDown board row col match
        diagRightUp     = checkDiagRightUp board row col match
        diagRightDown   = checkDiagRightDown board row col match
        diagLeftUp      = checkDiagLeftUp board row col match
        diagLeftDown    = checkDiagLeftDown board row col match 

        in
            ((right + left + up + down + diagRightUp + diagRightDown + diagLeftUp + diagLeftDown) `div` match)


-- maxNumAdjacent board row col match:
-- Return the largest number of adjacent tokens of that matching token, given a board and a row and column
maxNumAdjacent :: [[Int]] -> Int -> Int -> Int -> Int
maxNumAdjacent board row col match =
    let 
        right           = checkRight board row col match
        left            = checkLeft board row col match
        up              = checkUp board row col match
        down            = checkDown board row col match
        diagRightUp     = checkDiagRightUp board row col match
        diagRightDown   = checkDiagRightDown board row col match
        diagLeftUp      = checkDiagLeftUp board row col match
        diagLeftDown    = checkDiagLeftDown board row col match 

        in
            -- maxInt (right : left : up : down : diagRightUp : diagRightDown : diagLeftUp : diagLeftDown)
            maxInt [right, left , up, down, diagRightUp, diagRightDown, diagLeftUp, diagLeftDown]

-- maxInt lst:
-- Returns the maximum number in lst
maxInt :: [Int] -> Int
maxInt lst = foldl (\x y -> if x >= y then x else y) 0 lst
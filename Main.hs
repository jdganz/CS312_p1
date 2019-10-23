module MainShaun where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game
import ConnectFour

-- define the window
window = InWindow "Connect Four" (900, 900) (100, 100)

-- connect 4 grid height
winheight :: Float
winheight = 700


--connect 4 grid width
winwidth :: Float
winwidth = 700

--some constants
bgcolor = (makeColorI 50 50 50 255)
nodesize = 20
emptynodecolor = (makeColorI 200 200 200 255)
nodecolor1 = (makeColorI 255 50 50 255)
nodecolor2 = (makeColorI 255 255 50 255)

--function to make render the EndOfGame world state
gameAsPicture :: Result -> Picture
gameAsPicture (EndOfGame val totals playerType playerTurn)
    | val == 1 = pictures [ color nodecolor1 (circleSolid 80),
                            Translate (-140) 100 (color nodecolor1 (Scale 0.5 0.5 (Text "Red wins!"))), 
                            Translate (-65) 0 (color (greyN 0.0) (Scale 0.2 0.2 (Text "Play Again")))]
    | val == 0 = pictures [ color (greyN 0.5) (circleSolid 80),
                            Translate (-140) 100 (color (greyN 0.5) (Scale 0.5 0.5 (Text "It's a tie."))), 
                            Translate (-65) 0 (color (greyN 0.0) (Scale 0.2 0.2 (Text "Play Again")))]
    | otherwise = pictures [ color nodecolor2 (circleSolid 80), 
                            Translate (-160) 100 (color nodecolor2 (Scale 0.5 0.5 (Text "Yellow wins!"))), 
                            Translate (-65) 0 (color (greyN 0.0) (Scale 0.2 0.2 (Text "Play Again")))]

-- function that renders the ContinueGame world state (the game grid and pieces)
gameAsPicture (ContinueGame (State (board, whosturn) actions) totals playerType playerTurn)
    | whosturn == 1 = pictures [ colAsPicture (board !! 0) 0, 
                                 colAsPicture (board !! 1) 1, 
                                 colAsPicture (board !! 2) 2 , 
                                 colAsPicture (board !! 3) 3, 
                                 colAsPicture (board !! 4) 4, 
                                 colAsPicture (board !! 5) 5, 
                                 colAsPicture (board !! 6) 6, 
                                 translate 0 400 (color nodecolor1 (circleSolid nodesize)) ]
    | whosturn == 2 = pictures [ colAsPicture (board !! 0) 0, 
                                 colAsPicture (board !! 1) 1, 
                                 colAsPicture (board !! 2) 2, 
                                 colAsPicture (board !! 3) 3, 
                                 colAsPicture (board !! 4) 4, 
                                 colAsPicture (board !! 5) 5, 
                                 colAsPicture (board !! 6) 6, 
                                 translate 0 400 (color nodecolor2 (circleSolid nodesize)) ]

-- function that renders the ChoosePlayer world state
gameAsPicture (ChoosePlayer totals playerType playerTurn) = pictures [
                                                Translate (-10) 210 (color nodecolor1 (rectangleSolid 200 30)),
                                                Translate (-100) 200 (color nodecolor2 (Scale 0.2 0.2 (Text "Back to Menu"))),
                                                Translate (-220) 100 (color nodecolor2 (Scale 0.35 0.35 (Text "Choose your player"))),
                                                Translate (-200) (-100) (color nodecolor1 (circleSolid 80)), 
                                                Translate (-275) 0 (color nodecolor1 (Scale 0.3 0.3 (Text "Player 1"))), 
                                                Translate (-235) (-100) (color nodecolor2 (Scale 0.2 0.2 (Text "Select"))),
                                                Translate (-260) (-220) (color nodecolor1 (Scale 0.2 0.2 (Text ("TOTAL: " ++ show(totals !! 0))))),
                                                Translate (200) (-100) (color nodecolor2 (circleSolid 80)), 
                                                Translate (125) 0 (color nodecolor2 (Scale 0.3 0.3 (Text "Player 2"))), 
                                                Translate (165) (-100) (color nodecolor1 (Scale 0.2 0.2 (Text "Select"))),
                                                Translate (140) (-220) (color nodecolor2 (Scale 0.2 0.2 (Text ("TOTAL: " ++ show(totals !! 1)))))]

-- function that renders the StartGame world state
gameAsPicture (StartGame totals playerType playerTurn) = pictures [ Translate (-190) 200 (color nodecolor1 (Scale 0.5 0.5 (Text "CONNECT 4"))),
                                            Translate (-140) 100 (color nodecolor2 (Scale 0.2 0.2 (Text "Chose a game mode:"))),
                                            color nodecolor1 (circleSolid 80),
                                            Translate (-30) 20 (color (greyN 0.0) (Scale 0.15 0.15 (Text "Two"))),
                                            Translate (-30) 0 (color (greyN 0.0) (Scale 0.15 0.15 (Text "Player"))),
                                            Translate (-30) (-20) (color (greyN 0.0) (Scale 0.15 0.15 (Text "Game"))),         -- Draw the 2player game button
                                            Translate (-160) (-160) (color cyan (circleSolid 80)),
                                                    Translate (-200) (-140) (color (greyN 0.0) (Scale 0.15 0.15 (Text "Vs"))),
                                                    Translate (-200) (-160) (color (greyN 0.0) (Scale 0.15 0.15 (Text "Easy"))),
                                                    Translate (-200) (-180) (color (greyN 0.0) (Scale 0.15 0.15 (Text "Computer"))),
                                            Translate 0 (-160) (color cyan (circleSolid 80)), Translate (-40) (-140) (color (greyN 0.0) (Scale 0.15 0.15 (Text "Vs"))),
                                                    Translate (-40) (-160) (color (greyN 0.0) (Scale 0.15 0.15 (Text "Medium"))),
                                                    Translate (-40) (-180) (color (greyN 0.0) (Scale 0.15 0.15 (Text "Computer"))),
                                            Translate 160 (-160) (color cyan (circleSolid 80)), Translate 120 (-140) (color (greyN 0.0) (Scale 0.15 0.15 (Text "Vs"))),
                                                    Translate 120 (-160) (color (greyN 0.0) (Scale 0.15 0.15 (Text "Hard"))),
                                                    Translate 120 (-180) (color (greyN 0.0) (Scale 0.15 0.15 (Text "Computer"))),

                          Translate (-400) (-300) (color (greyN 1) (Scale 0.15 0.15 (Text "Note: When VSing computer: click on their turn to have computer drop a token")))]

-- function to draw a column of board bieces
colAsPicture :: [Int] -> Float -> Picture
colAsPicture col c = pictures [ nodeAsPicture (col !! 0) 0 c,
                                nodeAsPicture (col !! 1) 1 c,
                                nodeAsPicture (col !! 2) 2 c, 
                                nodeAsPicture (col !! 3) 3 c,
                                nodeAsPicture (col !! 4) 4 c,
                                nodeAsPicture (col !! 5) 5 c ]

-- function to draw a single board piece
nodeAsPicture :: Int -> Float -> Float -> Picture
nodeAsPicture node c r
    | node == 0 = translate (winheight/6 * r - 350 ) (winwidth - winwidth/7 * c - 450) (color emptynodecolor (Circle nodesize))
    | node == 1 = translate (winheight/6 * r - 350 ) (winwidth - winwidth/7 * c - 450) (color nodecolor1 (circleSolid nodesize))
    | node == 2 = translate (winheight/6 * r - 350 ) (winwidth - winwidth/7 * c - 450) (color nodecolor2 (circleSolid nodesize))

-- function to update game state when you click (to drop a piece)
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) (ContinueGame (State (board, whosturn) actions) totals playerType playerTurn)
    | (playerType == "Player" && elem (mousePosAsCol mousePos) actions) = connect4 (mousePosAsCol mousePos) (State (board, whosturn) actions) totals playerType playerTurn
    | (elem (mousePosAsCol mousePos) actions && whosturn == playerTurn) = connect4 (mousePosAsCol mousePos) (State (board, whosturn) actions) totals playerType playerTurn
    | (playerType == "EasyAI" && whosturn /= playerTurn) = connect4 (easyAI (State (board, whosturn) actions)) (State (board, whosturn) actions) totals playerType playerTurn
    | (playerType == "MediumAI" && whosturn /= playerTurn) = connect4 (mediumAI (State (board, whosturn) actions)) (State (board, whosturn) actions) totals playerType playerTurn
    | (playerType == "HardAI" && whosturn /= playerTurn) = connect4 (mediumAI (State (board, whosturn) actions)) (State (board, whosturn) actions) totals playerType playerTurn
    | otherwise = (ContinueGame (State (board, whosturn) actions) totals playerType playerTurn)

-- function to update game state from the EndOfGame state to a new game when you click play again
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) (EndOfGame val totals playerType playerTurn)
    | mouseOnCenter mousePos = (StartGame totals playerType playerTurn)
    | otherwise = (EndOfGame val totals playerType playerTurn)

-- function to update game state from the StartGame state to a new game when you click play again
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) (StartGame totals playerType playerTurn)
    | mouseOnVsEasyComputerButton mousePos = (ChoosePlayer totals "EasyAI" playerTurn)
    | mouseOnVsMediumComputerButton mousePos = (ChoosePlayer totals "MediumAI" playerTurn)
    | mouseOnVsHardComputerButton mousePos = (ChoosePlayer totals "MediumAI" playerTurn)
    | mouseOn2PlayerButton mousePos = (ChoosePlayer totals "Player" playerTurn)
    | otherwise = (StartGame totals playerType playerTurn)

-- function to update game state from the ChoosePlayer state to a new game when you click play again
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) (ChoosePlayer totals playerType playerTurn)
    | mouseOnPlayer1 mousePos = (ContinueGame connect4_start1 totals playerType 1)
    | mouseOnPlayer2 mousePos = (ContinueGame connect4_start2 totals playerType 2)
    | mouseOnMenu mousePos = (StartGame totals playerType playerTurn)
    | otherwise = (ChoosePlayer totals playerType playerTurn)

transformGame _ worldstate = worldstate

-- convert a mouse position to a column of the game grid
mousePosAsCol :: (Float, Float) -> Int
mousePosAsCol (x, y) = floor (6*(x + 400)/winheight)

-- check if a mouse position is on the play again button
mouseOnCenter :: (Float, Float) -> Bool
mouseOnCenter (x, y) = x > (-80) && x < 80 && y > (-80) && y < 80

-- check if a mouse position is on the player 1 button
mouseOnPlayer1 :: (Float, Float) -> Bool
mouseOnPlayer1 (x, y) = x > (-280) && x < (-120) && y > (-180) && y < (-20)

-- check if a mouse position is on the player 2 button
mouseOnPlayer2 :: (Float, Float) -> Bool
mouseOnPlayer2 (x, y) = x > 120 && x < 280 && y > (-180) && y < (-20)

-- check if a mouse position is on the menu button
mouseOnMenu :: (Float, Float) -> Bool
mouseOnMenu (x, y) = x > -100 && x < 100 && y > 180 && y < 220

-- check if a mouse position is on the play again button
mouseOn2PlayerButton :: (Float, Float) -> Bool
mouseOn2PlayerButton (x, y) = (x > (-80)) && (x < 80) && (y > (-80)) && (y < 80)

-- check if a mouse position is on the Play VS Computer Button:
mouseOnVsEasyComputerButton :: (Float, Float) -> Bool
mouseOnVsEasyComputerButton (x, y) = (x > (-240)) && (x < (-80))&& (y > (-240)) && (y < (-80))

mouseOnVsMediumComputerButton :: (Float, Float) -> Bool
mouseOnVsMediumComputerButton (x, y) = (x > (-80)) && (x < 80) && (y > (-240)) && (y < (-80))

mouseOnVsHardComputerButton :: (Float, Float) -> Bool
mouseOnVsHardComputerButton (x, y) = (x > 80) && (x < 240) && (y > (-240)) && (y < (-80))

-- world state is of type Result
main :: IO ()
main = play window bgcolor 30 (StartGame [0,0] "" 0) gameAsPicture transformGame (const id)
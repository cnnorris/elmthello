module Main where


import Board exposing (..)
import Signal exposing (Mailbox, mailbox)
import Window
import Graphics.Element as E exposing (image)
import Graphics.Input exposing (customButton, button, checkbox)
import Color
import Text as T
import Time
import List exposing (member, map, length)

import MiniMax exposing (..)

type alias State = (Int, Board, Bool) -- Int is player # whose turn it is, Bool is whether or not AI is on

-- Signal from buttons, Loc is a piece, Check is a timer reminder, AI is the AI toggle
type Sig = Loc (Int,Int) | Check Float | AI

initState = (1, initBoard, False)
aiMove = getAiMove

-- Generalized function to change turns
flipTurn a = case a of
                    1 -> 2
                    2 -> 1
                    _ -> Debug.crash "No turn"

-- Upstate: Takes in signals and reacts to them
-- Loc (-1,-1) is the signal sent by the "new game" button
-- AI is the signal sent by the AI toggle
-- Loc (x,y) is the signal sent by board buttons
-- Handles turn changes and whether or not the AI is running
upstate : Sig -> State -> State    -- Takes a clicked square and a state, updates if legal
upstate s (turn, board, ai) =  if s == Loc (-1,-1) then initState
                               else if s == AI then (turn, board, not ai)
                               else checkState <|
                                    case turn of
                                        3 -> (turn, board, ai)
                                        2 -> if ai then
                                                 case (aiMove turn board) of
                                                          Nothing -> (1, board, ai)
                                                          Just move -> (1, executeMove move 2 board, ai)
                                             else
                                                evaluateMove s (turn, board, ai)
                                        1 -> evaluateMove s (turn, board, ai)
                                        _ -> Debug.crash "Invalid turn"

-- Evaluate move alters the board state after the human player has chosen their move
-- Generalized to work regardless of p1 or p2
evaluateMove : Sig -> State -> State
evaluateMove s (turn, board, ai) =
    case s of
        Loc (x,y) ->
             if member (x,y) (legalMoves turn board) then
                (flipTurn turn, executeMove (x,y) turn board, ai)
             else
                (turn,board, ai)
        _    -> (turn,board, ai)


-- checkState tests for endgame, and whether or not the player whose turn it is has possible moves
checkState : State -> State
checkState (turn, board, ai) = case turn of
                             3 -> (turn, board, ai)
                             _ -> if length (legalMoves turn board) > 0 then
                                        (turn,board, ai)
                                  else if length (legalMoves (flipTurn turn) board) > 0 then
                                        (flipTurn turn, board, ai)
                                  else
                                        (3, board, ai)


-- Mailbox for board buttons, new game
buttonMailbox : Mailbox (Int,Int)
buttonMailbox = mailbox (0,0)

-- Mailbox for AI toggle
aiMailbox : Mailbox Bool
aiMailbox = mailbox True

-- Draw a button given a tile
toButton : Int -> Int -> State-> Tile ->  E.Element
toButton x y (t,b, ai) (T a loc)  = case a of
                            1 -> customButton (Signal.message buttonMailbox.address loc)
                                  (image x y "./img/player1.jpg")
                                  (image x y "./img/player1.jpg")
                                  (image x y "./img/player1.jpg")
                            2 -> customButton (Signal.message buttonMailbox.address loc)
                                  (image x y "./img/player2.jpg")
                                  (image x y "./img/player2.jpg")
                                  (image x y "./img/player2.jpg")

                            _ -> if member loc (legalMoves t b) && (t == 1  || not ai) then
                                    customButton (Signal.message buttonMailbox.address loc)
                                    (image x y "./img/default.jpg")
                                    (image x y "./img/mouseover.jpg")
                                    (image x y "./img/click.jpg")
                                 else
                                    customButton (Signal.message buttonMailbox.address loc)
                                    (image x y "./img/defaultnone.jpg")
                                    (image x y "./img/defaultnone.jpg")
                                    (image x y "./img/defaultnone.jpg")


-- Side panel, gives score, turn/endgame, AI status
description  : Int -> State -> E.Element
description h (turn, board, ai) = E.flow E.down
                [(E.container 200 (h//8) E.middle
                        <| E.width 200 <| E.justified <| T.fromString
                        <| "   Red: " ++ (toString (countBoardTiles 1 board))),
                 (E.container 200 (h//8) E.middle
                        <| E.width 200 <| E.justified <| T.fromString
                        <|"   Blue: " ++ (toString (countBoardTiles 2 board))),
                 (E.container 200 (h//2) E.middle
                        <| E.width 200 <| describeState (turn, board, ai) h),
                 (E.flow E.right
                    [E.container 60 60 E.middle (checkbox (Signal.message aiMailbox.address) ai),
                     E.container 80 60 E.middle
                                    <| E.width 80 <| E.justified <| T.fromString " Toggle AI",
                     E.container 200 60 E.middle
                                    <| E.width 200 <| E.justified <| T.fromString <|
                                        if ai then "(Currently ON)"
                                        else "(Currently OFF)"])

                 ]




-- Picks the correct image to put in the description panel
describeState :  State -> Int -> E.Element
describeState (turn, _ , ai) h =  E.flow E.right
                                            [ E.spacer 10 100,
                                             case turn of
                                                1 -> (image 180 120 "./img/redTurn.jpg")

                                                2 -> if ai then
                                                        image 180 120 "./img/aiTurn.jpg"
                                                     else
                                                        image 180 120 "./img/blueTurn.jpg"

                                                _ -> customButton (Signal.message buttonMailbox.address (-1,-1))
                                                    (image 180 120 "./img/doneDefault.jpg")
                                                    (image 180 120 "./img/doneOver.jpg")
                                                    (image 180 120 "./img/doneClick.jpg")]


-- Draws board and description panel
view :  State -> (Int, Int) -> E.Element
view (turn, board, ai) (w,h) = E.beside (E.flow E.down (map (\a -> E.flow E.right <|
                                                map (toButton ((min h w)//8) ((min h w)//8) (turn, board, ai)) a) board))
                                        (description h (turn, board, ai))


-- Starts and handles signal stream
-- Signals get thrown:
--     whenever player clicks
--     whenever AI is toggled
--     every 3 seconds, to re-wake up AI if it has to move twice in a row
stateOverTime : Signal State
stateOverTime = Signal.foldp upstate initState
                        (Signal.merge
                            (Signal.merge (Signal.map Loc buttonMailbox.signal) (Signal.map Check <| Time.every (3 * Time.second)))
                            (Signal.map (\a -> AI) aiMailbox.signal))

main = Signal.map2 view stateOverTime Window.dimensions















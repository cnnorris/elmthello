module Main where


import Board exposing (..)
import Signal exposing (Mailbox, mailbox)
import Window
import Graphics.Element as E exposing (image)
import Graphics.Input exposing (customButton)
import Color
import Text as T
import Time
import List exposing (member, map, length)

import MiniMax exposing (..)

type alias State = (Int, Board) -- Int is player # whose turn it is

type Sig = Loc (Int,Int) | Check Float

initState = (1, initBoard)
aiMove = getAiMove

flipTurn a = case a of
                    1 -> 2
                    2 -> 1
                    _ -> Debug.crash "No turn"

upstate : Sig -> State -> State    -- Takes a clicked square and a state, updates if legal
upstate s (turn, board) =   checkState <|
                                case turn of
                                    3 -> (turn,board)
                                    2 -> case (aiMove turn board) of
                                                      Nothing -> (1, board)
                                                      Just move -> (1, executeMove move 2 board)
                                    1 -> case s of
                                            Loc (x,y) ->
                                                 if member (x,y) (legalMoves turn board) then
                                                    (2, executeMove (x,y) turn board)
                                                 else
                                                    (turn,board)
                                            _    -> (turn,board)
                                    _ -> Debug.crash "Invalid turn"


checkState : State -> State
checkState (turn, board) = case turn of
                             3 -> (turn,board)
                             _ -> if length (legalMoves turn board) > 0 then
                                    if turn == 2 then let x = Signal.send buttonMailbox.address (4,4) in
                                            (turn,board)
                                    else
                                        (turn,board)
                                  else if length (legalMoves (flipTurn turn) board) > 0 then
                                        (flipTurn turn, board)
                                  else
                                        (3, board)



buttonMailbox : Mailbox (Int,Int)
buttonMailbox = mailbox (0,0)

toButton : Int -> Int -> State-> Tile ->  E.Element
toButton x y (t,b) (T a loc)  = case a of
                            1 -> customButton (Signal.message buttonMailbox.address loc)
                                  (image x y "/player1.jpg")
                                  (image x y "/player1.jpg")
                                  (image x y "/player1.jpg")
                            2 -> customButton (Signal.message buttonMailbox.address loc)
                                  (image x y "/player2.jpg")
                                  (image x y "/player2.jpg")
                                  (image x y "/player2.jpg")

                            _ -> if member loc (legalMoves t b) && t == 1 then
                                    customButton (Signal.message buttonMailbox.address loc)
                                    (image x y "/default.jpg")
                                    (image x y "/mouseover.jpg")
                                    (image x y "/click.jpg")
                                 else
                                    customButton (Signal.message buttonMailbox.address loc)
                                    (image x y "/defaultnone.jpg")
                                    (image x y "/defaultnone.jpg")
                                    (image x y "/defaultnone.jpg")



description  : Int -> State -> E.Element
description h (turn, board) = E.flow E.down
                [(E.container 200 (h//8) E.middle
                        <| E.width 200 <| E.justified <| T.fromString
                        <| "   Red: " ++ (toString (countBoardTiles 1 board))),
                 (E.container 200 (h//8) E.middle
                        <| E.width 200 <| E.justified <| T.fromString
                        <|"   Blue: " ++ (toString (countBoardTiles 2 board))),
                (E.container 200 (h//2) E.middle
                        <| E.width 200 <| E.justified <| T.fromString (
                        describeState turn))]

describeState : Int -> String
describeState turn = case turn of
                                 2 -> "   AI is moving"
                                 1 -> "   Red moves"
                                 _ -> "   Game over!"


view :  State -> (Int, Int) -> E.Element
view (turn, board) (w,h) = E.beside (E.flow E.down (map (\a -> E.flow E.right <| map (toButton ((min h w)//8) ((min h w)//8) (turn,board)) a) board)) (description h (turn,board))
-- If AI moves twice in a row, second turn doesn't get triggered



stateOverTime : Signal State
stateOverTime = Signal.foldp upstate initState
                        (Signal.merge (Signal.map Loc buttonMailbox.signal) (Signal.map Check <| Time.every (3 * Time.second)))

main = Signal.map2 view stateOverTime Window.dimensions
module Main where


import Board exposing (..)
import Signal exposing (Mailbox, mailbox)
import Window
import Graphics.Element as E
import Graphics.Input exposing (button)
import Color
import Text as T
import List exposing (member, map)

import MiniMax exposing (..)

type alias State = (Int, Board) -- Int is player # whose turn it is



initState = (1, initBoard)
aiMove = getAiMove

flipTurn a = case a of
                    1 -> 2
                    2 -> 1
                    _ -> Debug.crash "No turn"

upstate : (Int, Int) -> State -> State    -- Takes a clicked square and a state, updates if legal
upstate (x,y) (turn, board) = if member (x,y) (legalMoves turn board) then
                                                let board' = executeMove (x,y) turn board in
                                                case (aiMove turn board) of
                                                      Nothing -> (flipTurn turn, board')
                                                      Just move -> (turn, executeMove move turn board)
                                              else
                                                (turn, board)



buttonMailbox : Mailbox (Int,Int)
buttonMailbox = mailbox (0,0)

toButton : Tile -> E.Element
toButton (T a x) = button (Signal.message buttonMailbox.address x) (toString a)


view :  State -> (Int, Int) -> E.Element
view (turn, board) (w,h) = E.flow E.down <| (E.show (toString turn)) :: (map (\a -> E.flow E.right <| map toButton a) board)


stateOverTime : Signal State
stateOverTime = Signal.foldp upstate initState buttonMailbox.signal

main = Signal.map2 view stateOverTime Window.dimensions
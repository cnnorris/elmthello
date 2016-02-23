module Board where

-- Board.elm
{-
    This file will be used for the GUI functions and graphics  
-} 


import Window
import Color
import Text as T
import Graphics.Element as E
import Graphics.Collage as C exposing (defaultLine)
import Graphics.Input exposing (button, customButton)
import Time exposing (Time)
import Signal exposing (Mailbox, mailbox)


boardSize = 8

type Tile = T Int (Int, Int)  -- Player/color, (row,col) 

type alias Button = () -- Not implemented yet
type alias Square = (Button, Tile)   -- Button is the thing that controls UI, Space is state of square
type alias Row   = List Tile
type alias Board = List Row






type alias State = (Int, Board) -- Int is player # whose turn it is

{-

flipLocation (x,y) board =
        (take x board) ++  
            ((take y board) ++ [ flipTile <| getSpace (x,y) board ] ++ (drop (y+1) board)
             :: (drop (x+1) board))

flipTile : Tile -> Tile
flipTile (T c (x,y)) = case c of 
                        0 -> Debug.crash "Invalid flip"
                        1 -> T 2 (x,y)
                        2 -> T 1 (x,y)





-- legalMoves takes a Bool player identifier and a board, returns legal moves by that player
legalMoves : Bool -> Board -> List Space
legalMoves player board =  {- remove duplicates <| -} foldr (++) [] <| map (\a -> legalMovesForRow player a board) board

legalMovesForRow player row board = filter (\a -> legalMoveForSpace a player board) row

legalMoveForSpace (b, (T p loc)) player board = 
             if map (\a -> (checkDirection a player board (a loc))) movements |>
                filter ((==) 2) |> length |> ((>) 0) then
                True
             else
                False
             

movements =    [(\(a,b) -> (a+1,b)),
                (\(a,b) -> (a-1,b)),
                (\(a,b) -> (a+1,b+1)),
                (\(a,b) -> (a-1,b+1)),
                (\(a,b) -> (a+1,b-1)),
                (\(a,b) -> (a-1,b-1)),
                (\(a,b) -> (a,b+1)),
                (\(a,b) -> (a,b-1))]

-- Returns 2 if there is a legal move for player p2 based on the tiles above 
checkDirection : ((Int,Int) -> (Int,Int)) -> Int -> (Int, Int) -> Board -> Int
checkDirection move player (x,y) board = 
            if x < boardSize - 1 && y < boardSize -1 then
                let (b, T c _) = spaceAt (x, y) board in
                if c == player then
                    1
                else if c == 0 then
                    0
                else
                    let rest  = checkDirection move player (move (x, y)) board in 
                    if rest == 1 || rest == 2 then
                        2
                    else
                        0
            else
                0
                   

flipDirection : ((Int,Int) -> (Int,Int)) -> Int -> (Int, Int) -> Board -> (Int , Board)
flipDirection move player (x,y) board =
           if x < boardSize - 1 && y < boardSize -1 then
                let (b, T c _) = spaceAt (x, y) board in
                if c == player then
                    (1, board)
                else if c == 0 then
                    (0, board)
                else
                    let rest, board'  = flipDirection player (move (x, y)) board in
                    if rest == 1 || rest == 2 then
                        (2, flipLocation (x,y) board)
                    else
                        (0, board)
            else
                (0, board)



executeMove (x,y) player board = snd <| foldr (\a (b,c) -> flipDirection a player (a (x,y)) b) (0, board) movements



head_ : List a -> a
head_ a = case (head a) of :
            Nothing -> Debug.crash "Head_"
            Just k  -> k 

-- 0 - indexed, meaning 1'st element is the head after drop 0
spaceAt : (Int, Int) -> Board -> Square  
spaceAt (x,y) board = drop x board |> head_ |> drop y |> head_


upstate : Square -> State -> State    -- Takes a clicked square and a state, updates if legal
upstate (button, T p (x,y)) (turn, board) -> if p == turn && (x,y) in (legalMoves turn board) then
                                                let board' = executeMove (x,y) turn board in
                                                case aiMove of 
                                                      Nothing -> (turn, board)
                                                      Just board'' -> (turn, board'')
                                              else
                                                (turn,board)       


view : (Int, Int) -> State -> Element

main : Signal Element
main 

stateOverTime : Signal State
stateOverTime = Signal.foldp upstate initState buttonMailbox.signal
    
                        




aiMove : Board -> Maybe Tile 

-}
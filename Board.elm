module Board where

-- Board.elm
{-
    This file will be used for board functionality and game rules
-}


import Window
import Color
import Text as T
import Graphics.Element as E
import Graphics.Collage as C exposing (defaultLine)
import Graphics.Input exposing (button, customButton)
import Time exposing (Time)
import Signal exposing (Mailbox, mailbox)

import List exposing (drop, take, foldr, map, head, length, filter)

-- This could increase I guess
boardSize = 8

type Tile = T Int (Int, Int)  -- Player/color, (row,col)
type alias Row   = List Tile
type alias Board = List Row


-- Used to set a particular tile to a particular player
setBoard : (Int,Int) -> Int -> Board -> Board
setBoard (x,y) player board = let r = head_ <| drop x board in
        (take x board) ++  
            (((take y r) ++ [(T player (x,y)) ] ++ (drop (y+1) r))
             :: (drop (x+1) board))


-- Removes type wrapper around coordinates
unTile : List (List Tile) -> List (List Int)
unTile b = let f = (\(T x _) ->  x) in
                map (map f) b


makeRow : Int -> Int -> Row
makeRow rowNum counter = case counter of
  0 -> []
  _ -> (T 0 (rowNum, boardSize-counter)) :: (makeRow rowNum (counter-1))

makeBoard : Int -> Board
makeBoard c = case c of
  0 -> []
  _ -> (makeRow (boardSize-c) boardSize) :: (makeBoard (c-1)) 

initBoard = makeBoard boardSize |> setBoard (3,3) 1 |> setBoard (4,4) 1 |>
                    setBoard (4,3) 2 |> setBoard (3,4) 2


-- Flips tile at given location
flipLocation (x,y) board = let r = head_ <| drop x board in
        (take x board) ++  
            (((take y r) ++ [( flipTile <| spaceAt (x,y) board) ] ++ (drop (y+1) r))
             :: (drop (x+1) board))

-- Reassigns player value of tile
flipTile : Tile -> Tile
flipTile (T c (x,y)) = case c of
                        0 -> Debug.crash "Invalid flip"
                        1 -> (T 2 (x,y))
                        2 -> (T 1 (x,y))
                        _ -> Debug.crash "Too many players...?"




-- legalMoves takes a Bool player identifier and a board, returns legal moves by that player
-- A move is legal if there are opposite colored tiles between it and a similarly colord tile
-- in cardinal or semicardinal directions
legalMoves : Int -> Board -> List (Int, Int)
legalMoves player board = map (\(T _ a) -> a) <| foldr (++) [] <| map (\a -> legalMovesForRow player a board) board

legalMovesForRow player row board = filter (\a -> legalMoveForSpace a player board) row
legalMoveForSpace (T p loc) player board =
            if p /= 0 then False else
            if map (\a -> (checkDirection a player (a loc) board)) movements |>
                filter ((==) 2) |> length |> ((<) 0) then
                True
             else
                False
             

-- These are all the cardinal/semicardinal directions, used to navigate board
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
            if x < boardSize && y < boardSize
               && x >= 0 && y >= 0 then
                let (T c _) = spaceAt (x, y) board in
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
                   
-- Flips all tiles in a given direction from chosen tile until similarly-colord tile
-- is found
flipDirection : ((Int,Int) -> (Int,Int)) -> Int -> (Int, Int) -> Board -> (Int , Board)
flipDirection move player (x,y) board =
           if x < boardSize  && y < boardSize  &&
              x >= 0 && y >= 0 then
                let (T c _) = spaceAt (x, y) board in
                if c == player then
                    (1, board)
                else if c == 0 then
                    (0, board)
                else
                    let (rest, board')  = flipDirection move player (move (x, y)) board in
                    if rest == 1 || rest == 2 then
                        (2, flipLocation (x,y) board')
                    else
                        (0, board)
            else
                (0, board)


-- Sets piece on board, then goes and flips from there
executeMove (x,y) player board = setBoard (x,y) player <| snd <| foldr (\a (b,c) -> flipDirection a player (a (x,y)) c) (0, board) movements


-- Un-maybe'd version of head
head_ : List a -> a
head_ a = case (head a) of
            Nothing -> Debug.crash "Head_"
            Just k  -> k 

-- 0 - indexed, meaning 1'st element is the head after drop 0
spaceAt : (Int, Int) -> Board -> Tile
spaceAt (x,y) board = drop x board |> head_ |> drop y |> head_


    

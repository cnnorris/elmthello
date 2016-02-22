module MiniMax where 
type MovesTree = Node (Int, Int) (List MovesTree)
type Tile = T Int (Int, Int)  -- Player/color, (row,col) 

type alias Button = () -- Not implemented yet
type alias Square = (Button, Tile)   -- Button is the thing that controls UI, Space is state of square
type alias Row   = List Tile
type alias Board = List Row

getMoveFromSquare (button, T a (x,y)) = (x,y)
npc = 2

-- create tree of simulated moves
-- ( a move is a pair of Ints representing coordinates)
-- int is 1 or 2 to represent player number
-- second int is height of tree
simMoveTrees : Int -> Int -> Board -> (Int, Int) -> MovesTree
simMoveTrees p h board move = 
    let
      board' = case move of 
        -- (-1, -1) means we're at the root of the tree
        (-1, -1) -> board
        _ -> executeMove move p board 
      -- lm = legalMoves p board'
      lm = legalMoves h board'
      opponent = case p of 
        1 -> 2
        _ -> 1
    in
    case h of 
      0 -> Node move []
      _ -> 
          case lm of
            [] -> Node move []
            _ -> Node move (List.map (\x -> simMoveTrees opponent (h-1) board' (getMoveFromSquare x)) lm) 
-- traverse tree of simulated moves to find best move
-- sim tree, player num, current board
getBestMove : MovesTree -> Int -> Board -> (Int, (Int, Int))
getBestMove mt p b = 
  case mt of
    -- at the leaf, we will have a different scoring algorithm; for now return 0
    Node move [] -> (0, move)
    Node move mts -> ((score p (List.map (\t -> getBestMove t p b) mts) b move), move)

-- function to score a move
--      have some heuristics 
--      use heuristics to define value of leaf nodes?
-- score : player num, board, move
-- score : Int Int Board (Int, Int)
score : Int -> List (Int, (Int,Int)) -> Board -> (Int, Int) -> Int
score p children b move = 
  let
    getMax scorepairs = case scorepairs of 
      -- negative infinity
      [] -> -100
      (score, move) :: scorepairs' -> max score (getMax scorepairs')
    getMin scorepairs = case scorepairs of 
      -- positive infinity
      [] -> 100
      (score, move) :: scorepairs' -> min score (getMin scorepairs')
  in
  if p == npc 
    then getMax children 
    else getMin children


-- stubbed out functions from Board.elm
boardSize = 8

executeMove : (Int, Int) -> Int -> Board -> Board
executeMove (x,y) p b = b 

legalMoves : Int -> Board -> List Square
legalMoves p b = [((), T 1 (p,0)), ((), T 1 (p,1))]

makeRow : Int -> Int -> Row
makeRow rowNum counter = case counter of
  0 -> []
  _ -> (T 0 (rowNum, boardSize-counter)) :: (makeRow rowNum (counter-1))

makeBoard : Int -> Board
makeBoard c = case c of
  0 -> []
  _ -> (makeRow (boardSize-c) boardSize) :: (makeBoard (c-1)) 

initBoard = makeBoard boardSize


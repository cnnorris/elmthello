module MiniMax where 

import Board exposing (boardSize, Tile, Row, Board, legalMoves, initBoard, executeMove)

type MovesTree = Node (Int, Int) (List MovesTree)

getMoveFromTile (Board.T a (x,y)) = (x,y)
npc = 2
human = 1
depth = 4


getAiMove : Int -> Board -> Maybe (Int,Int)
getAiMove t b = 
  let
    simTree = simMoveTrees npc depth b (-1, -1)
    bestMove = getBestMove simTree npc b
  in
    Just (snd bestMove)

-- create tree of simulated moves
-- ( a move is a pair of Ints representing coordinates)
-- p is 1 or 2 to represent player number
-- second int is height of tree
simMoveTrees : Int -> Int -> Board -> (Int, Int) -> MovesTree
simMoveTrees p h board move = 
    let
      board' = case move of 
        -- (-1, -1) means we're at the root of the tree
        (-1, -1) -> board
        _ -> executeMove move p board 
      opponent = case move of 
        (-1, -1) -> p
        _ -> case p of 
          1 -> 2
          _ -> 1
    in
    case h of 
      0 -> Node move []
      _ -> 
          case (legalMoves opponent board') of
            [] -> Node move []
            _ -> Node move (List.map (\x -> simMoveTrees opponent (h-1) board' x) (legalMoves opponent board'))


-- traverse tree of simulated moves to find best move
-- sim tree, player num, current board
getBestMove : MovesTree -> Int -> Board -> (Int, (Int, Int))
getBestMove mt p b = 
  case mt of
    -- at the leaf, we will have a different scoring algorithm; for now return 0
    Node move [] -> (scoreBoard b, move)
    Node move mts -> case move of 
      (-1, -1) -> (score p (List.map (\t -> getBestMove t p b) mts) b move)
      _ -> ((fst (score p (List.map (\t -> getBestMove t p b) mts) b move)), move)

-- function to score a board (at the leaves of the MoveTree)
-- Idea: in the future, I should take into account whose turn is next?
scoreBoard : Board -> Int
scoreBoard b = 
  let
    win = verifyWinner b
    advantage = (countTiles npc b) - (countTiles human b)
  in
  case win of
    Just (winner, _) -> if winner == npc then 1000 else if winner == human then -1000 else 0
    Nothing -> advantage -- expand to include more heuristics here


-- Is there a winner? Just (playerNum, magnitude of advantage)
-- in the case of tie, we return Just (0, 0)
verifyWinner : Board -> Maybe (Int, Int)
verifyWinner b = 
  let
  -- check if there are still moves
  lm_npc = legalMoves npc b
  lm_human = legalMoves human b
  diff = (countTiles npc b) - (countTiles human b)
  in
    case lm_npc of 
      [] -> case lm_human of
        [] -> if diff == 0 then Just (0, 0)
              else if diff > 0 then Just (npc, diff)
              else Just (human, (-1*diff))
        _ -> Nothing
      _ -> Nothing

countTiles : Int -> Board -> Int
countTiles p b =
  List.foldr (+) 0 
    (List.map (\row -> List.length (List.filter (\(Board.T a (x,y)) -> if a == p then True else False) row)) b)

-- function to score a move
--      have some heuristics 
--      use heuristics to define value of leaf nodes?
-- score : player num, children, board, move
score : Int -> List (Int, (Int,Int)) -> Board -> (Int, Int) -> (Int,(Int, Int))
score p children b move = 
  let
    getMax scorepairs = case scorepairs of 
      -- negative infinity
      [] -> (-10000, (-1, -1))
      (score, move) :: scorepairs' -> if (max score (fst(getMax scorepairs'))) == score then (score,move) else (getMax scorepairs')
    getMin scorepairs = case scorepairs of 
      -- positive infinity
      [] -> (10000, (-1,-1))
      (score, move) :: scorepairs' -> if (min score (fst(getMin scorepairs'))) == score then (score,move) else (getMin scorepairs')
  in
  if p == npc 
    then getMax children 
    else getMin children




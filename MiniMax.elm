module MiniMax where 

import Board exposing (boardSize, Tile, Row, Board, legalMoves, initBoard, executeMove, spaceAt)

type MovesTree = Node (Int, Int) (List MovesTree)

getMoveFromTile (Board.T a (x,y)) = (x,y)
npc = 2
human = 1
depth = 4

cornerBias = 100
goodEdgeBias = 50
edgeSetUpBias = -50
cornerSetUpBias = -100
normalBias = 5

getAiMove : Int -> Board -> Maybe (Int,Int)
getAiMove t b = 
  let
    simTree = simMoveTrees npc depth b (-1, -1)
    bestMove = getBestMove simTree npc b
  in
    Just (snd bestMove)

coordsToTiles : List (Int ,Int) -> Board -> List Tile 
coordsToTiles coords b = List.map (\x -> spaceAt x b) coords

getPlayer (Board.T a (x,y)) = a

countTiles : Int -> Board -> List (Int, Int) -> Int
countTiles p b cs =
  List.length (List.filter (\(Board.T a (x,y)) -> if a == p then True else False) (coordsToTiles cs b)) 


goodEdgeCoords : List (Int, Int) 
goodEdgeCoords = [(0,2),(0,3),(0,4),(0,5),(7,2),(7,3),(7,4),(7,5),(2,0),(3,0),(4,0),(5,0),(2,7),(3,7),(4,7),(5,7)]

cornerCoords : List (Int, Int)
cornerCoords = [(0,0),(7,0),(0,7),(7,7)]

cornerSetUpCoords : List (Int, Int)
cornerSetUpCoords = [(0,1),(0,6),(1,0),(1,1),(1,6),(1,7),(6,0),(6,1),(7,1),(6,6),(6,7),(7,6)]

edgeSetUpCoords : List (Int, Int)
edgeSetUpCoords = [(1,2),(1,3),(1,4),(1,5),(6,2),(6,3),(6,4),(6,5),(2,1),(3,1),(4,1),(5,1),(2,6),(3,6),(4,6),(5,6)]


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
            nextMoves -> 
              Node move (List.map (\x -> simMoveTrees opponent (h-1) board' x) (prune nextMoves board' p))

type SpecialBool = Corner | B Bool

isStable : (Int, Int) -> Int -> Board -> Bool
isStable (x,y) currPlayer b =
  let
    checkRight (x,y) = 
      if (x+1) > 7 then Corner
      else let p = getPlayer <| spaceAt (x+1,y) b in
        if p == currPlayer then (checkRight (x+1,y)) else if p == 0 then B True else B False
    checkLeft (x,y) = 
      if (x-1) < 0 then Corner
      else let p = getPlayer <| spaceAt (x-1,y) b in
        if p == currPlayer then (checkLeft (x-1,y)) else if p == 0 then B True else B False

    checkDown (x,y) =
      if (y+1) > 7 then Corner
      else let p = getPlayer <| spaceAt (x,y+1) b in
        if p == currPlayer then (checkDown (x,y+1)) else if p == 0 then B True else B False
    checkUp (x,y) =
      if (y-1) < 0 then Corner
      else let p = getPlayer <| spaceAt (x,y-1) b in
        if p == currPlayer then (checkUp (x,y-1)) else if p == 0 then B True else B False
  in
  if y == 0 || y == 7 then 
    case ((checkRight (x,y)), (checkLeft (x,y))) of 
      (Corner,_) -> True
      (_,Corner) -> True
      (B t, B s) -> t && s
  else 
    case ((checkDown (x,y)), (checkUp (x,y))) of 
      (Corner,_) -> True
      (_,Corner) -> True
      (B t, B s) -> t && s
    

-- add pruning based on heuristics.
prune : List (Int, Int) -> Board -> Int -> List (Int, Int)
prune possMoves b p =
  let
    cornerPrune pm = case pm of 
      [] -> []
      x :: xs -> if (List.member x cornerCoords) then [x] else (cornerPrune xs)
    -- I don't use the edgePrune as of now
    edgePrune pm board= case pm of 
      [] -> []
      x :: xs -> if (List.member x goodEdgeCoords) 
        then 
          if (isStable x p (executeMove x p b))
          then x :: (edgePrune xs board) 
          else (edgePrune xs board)
        else (edgePrune xs board)
    -- I don't use the cornerSetUpPrune as of now
    cornerSetUpPrune pm = case pm of 
      [] -> []
      x :: xs -> if (List.member x cornerSetUpCoords) then (cornerSetUpPrune xs) else x::(cornerSetUpPrune xs)
  in
    if List.length (cornerPrune possMoves) == 1 then (cornerPrune possMoves)
    else 
      let
       edgePruned = edgePrune possMoves b
      in 
      if (List.length edgePruned) > 0 then edgePruned
      else possMoves


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
    cornerCountDiff = (countTiles npc b cornerCoords) - (countTiles human b cornerCoords)
    cornerSetUpCountDiff = (countTiles npc b cornerSetUpCoords) - (countTiles human b cornerSetUpCoords)
    goodEdgeCountDiff = (countTiles npc b goodEdgeCoords) - (countTiles human b goodEdgeCoords)
    edgeSetUpCountDiff = (countTiles npc b edgeSetUpCoords) - (countTiles human b edgeSetUpCoords)
    advantage = (countBoardTiles npc b) - (countBoardTiles human b)
  in
  case win of
    Just (winner, _) -> if winner == npc then 1000 else if winner == human then -1000 else 0
    Nothing -> let otherDiff = (advantage-cornerCountDiff-cornerSetUpCountDiff-goodEdgeCountDiff-edgeSetUpCountDiff) in 
      cornerBias*cornerCountDiff+cornerSetUpBias*cornerSetUpCountDiff+goodEdgeCountDiff*goodEdgeBias+edgeSetUpCountDiff*edgeSetUpBias+normalBias*otherDiff


-- Is there a winner? Just (playerNum, magnitude of advantage)
-- in the case of tie, we return Just (0, 0)
verifyWinner : Board -> Maybe (Int, Int)
verifyWinner b = 
  let
  -- check if there are still moves
  lm_npc = legalMoves npc b
  lm_human = legalMoves human b
  diff = (countBoardTiles npc b) - (countBoardTiles human b)
  in
    case lm_npc of 
      [] -> case lm_human of
        [] -> if diff == 0 then Just (0, 0)
              else if diff > 0 then Just (npc, diff)
              else Just (human, (-1*diff))
        _ -> Nothing
      _ -> Nothing

countBoardTiles : Int -> Board -> Int
countBoardTiles p b =
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
  in let 
    (optScore, optMove) = if p == npc then getMax children else getMin children
    whoseTurn = if p == npc then 1 else -1
    in
      if List.member optMove cornerCoords then
        (optScore + whoseTurn*cornerBias,optMove)
      else if List.member optMove cornerSetUpCoords then
        (optScore + whoseTurn*cornerSetUpBias,optMove)
      else if List.member optMove edgeSetUpCoords then
        (optScore + whoseTurn*edgeSetUpBias,optMove)
      else if List.member optMove goodEdgeCoords then
        (optScore + whoseTurn*goodEdgeBias, optMove)
      else
        (optScore, optMove)



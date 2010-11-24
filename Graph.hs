module Graph where
import Data.Graph.Inductive
import Data.Graph.Analysis
import Data.Number.Symbolic
import Data.Maybe
import Data.Function (on)
import Char
import Data.List
import Control.Monad

import Theory
import Theory.Phi3
import Theory.Phi4
import Moment

type Modifier = [Char]
type Modified a = (a,[Modifier])

type DElement = Modified (DElemType, Moment)

data DNode = ENode
           | INode DElement deriving (Show)
type DLine = DElement

type Diagram = Gr DNode DLine

dia_empty = ([],0,ENode,[]) & (empty :: Diagram)

prop :: DLine
prop = ((DProp,emptyMoment),[])

vert :: DNode
vert = INode ((DVertex,emptyMoment),[])

contextsFromStr s =
  let (res,_,_,_) = foldl (flip parseChar) ([],[],[],1) s
  in res


parseChar '-' (ctxs,edgsIn,edgsOut,nr) = ((edgsIn,nr,vert,edgsOut) : ctxs,[],[],nr+1)
parseChar 'e' (ctxs,edgsIn,edgsOut,nr) = (ctxs,(prop,0):edgsIn,(prop,0):edgsOut,nr)
parseChar c (ctxs,edgsIn,edgsOut,nr) =
  let n = digitToInt c
      edgsIn' = (prop,n+1):edgsIn
      edgsOut' | nr == n+1 = edgsOut
               | otherwise = (prop,n+1):edgsOut
  in (ctxs,edgsIn',edgsOut',nr)


buildDiagramStr s = foldl (flip (&)) dia_empty (contextsFromStr s)

-- nickel from diagram
nickelDiagram' d p = foldl addAdj [] p where
  addAdj l (n,n') = l ++ sort [x | x <- adjNodes, x >= n' || x == 0] where
    adjNodes = mapMaybe (flip lookup p') (suc d n)
    p' = (0,0):p

nickelDiagram d =
  minimum [nickelDiagram' d x | x <- map (\p -> zip p [1..]) perms] where
  perms = permutations [x | x <- nodes d, x /= 0]
  
-- some diagram routines

-- is diagram biconnected?
isBCD :: Diagram -> Bool
isBCD = (== 1) . length . bcc . (delNode 0)

-- nr of tails
tailsNr :: Diagram -> Int
tailsNr = flip outdeg $ 0

-- subgraph with one node removed
removeOne :: Diagram -> Node -> Diagram
removeOne dia n =
  let (mctxs,d') = match n dia
      fixTails Nothing d = d
      fixTails (Just ctxs) d = foldl addTail d adj where
        (adj,_,_,_) = ctxs
        addTail d_ (_,0) = d_
        addTail d_ (_,k) | k == n = d_ -- ignore 1-loops
                         | otherwise =
                           insEdge (0,k,prop) $ insEdge (k,0,prop) d_
  in fixTails mctxs d'

-- make subgraph by removing list of nodes
mkSubgraph :: Diagram -> [Node] -> Diagram
mkSubgraph = foldl removeOne

-- generate all subgraphs (subgraphs with only one vertex left are ignored)
allSubgraphs :: Diagram -> [Diagram]
allSubgraphs d = map (mkSubgraph d) ls where
  ls = [x | x <- tail $ subsequences [1..n], length x < (n-1)]
  n = (noNodes d) - 1

-- biconnected subgraphs with desired number of tails (taken from theory definition)
signSubgraphs :: Theory -> Diagram -> [Diagram]
signSubgraphs th d =
  [x | x <- allSubgraphs d, isBCD x, (tailsNr x) `elem` (tailsSignDiagram th)]
     
-- some functions to deal with cycles in diagrams

-- test if cycles are the same
cycleEq :: (Eq a) => [a] -> [a] -> Bool
cycleEq c1 c2 | length c1 /= length c2 = False
              | otherwise =
                isInfixOf c1 c2' || isInfixOf (reverse c1) c2' where
                  c2' = c2 ++ c2
                  
-- compute the cycles in the diagram
-- cyclesIn' function from Graphalyze library doesn't suit our needs
-- because of the following:
-- a. It returns long (3 or more vertices) cycles twice (forward and backward)
-- b. It returns short (2 vertices) cycle for _every_ pair of
-- connected nodes
-- c. For every pair of short and long cycles such that both vertices
-- of the short cycle are on the long one it doesn't return symmetric
-- long cycle (with the same vertices but different edge)
-- d. It doesn't return 1-loops (self-connected vertices)
-- for problems b.-d. we use the representation in the form of
-- the list of egdes, each represented as ((Node,Node),Index) where
-- index is added to distinguish edges connecting the same vertices
-- TODO: test it with 1-loops!!!
cycles :: Diagram -> [([Node],[(Edge,Int)])]
cycles d = (cyclesLoops d) ++ (cyclesShort d) ++ (cyclesLong d)

-- "short" cycles. For each pair of vertices (a,b) with n edges
-- between them there are n(n-1)/2 cycles and n-1 of them are
-- independent

-- get list of pairs of vertices which have more than 1 edge between them
shortCyclesVertices d =
  map head [x | x <- group es, length x > 1] where
    es = [x | x <- edges d', fst x < snd x] -- condition removes duplicate edges and 1-loops
    d' = delNode 0 d

-- all short cycles. for every pair of vertices we take we get
-- a cycle for any (unordered) combination of edges
cyclesShort d =
  concatMap shortCycles2V (shortCyclesVertices d) where
    shortCycles2V e@(a,b) =
      zip (repeat [a,b]) [[(e,i1),(e',i2)] | i1 <- [1..n], i2 <- [1..n], i1 < i2] where  
        n = length [x | x <- edges d, x == e]
        e' = (b,a)

-- independent set of short cycles
cyclesShort' d =
  concatMap shortCycles2V' (shortCyclesVertices d) where
    shortCycles2V' e@(a,b) =
      zip (repeat [a,b]) cs where 
        cs = [[(e,i1),(e',i2)] | (i1,i2) <- zip [1..n] [2..n]]
        n = length [x | x <- edges d, x == e]
        e' = (b,a)

-- return "long" cycles (counting cycle with the same vertices only once
-- even if there are more than one edge)
cyclesLong' d =
  let d' = delNode 0 d
  in nubBy cycleEq [x | x <- cyclesIn' d', length x > 2]
     
-- and now add the missing "long" cycles (along with their respective
-- edge indices representation)
cyclesLong d = concatMap f (cyclesLong' d) where
  f x = zip (repeat x) (flip edgesToIndexed d . cycleToEdges $ x)
      
-- find 1-loops
cyclesLoops d = concatMap f vsWithLoops where
  vsWithLoops = nub [fst x | x <- edges d', fst x == snd x]
  d' = delNode 0 d
  f v = zip (repeat [v]) loopEdgs where
    loopEdgs = edgesToIndexed [(v,v)] d
    e = edges d

-- convert cycle from vertex to edge representation
cycleToEdges c =
  let (res,_) = foldr (\x (l,y) -> ((x,y):l,x)) ([],head c) c
  in res

-- add index to the edge to distinguish edges between the same
-- vertices (may return more than one edge)
edgeAddIndex e d = zip (repeat e) [1..n] where
  n = length [x | x <- edges d, x == e] 
  
-- convert cycle from edges written as (v1,v2) to list of
-- cycles with indexes added to edges
edgesToIndexed es d = foldl findEdges [[]] (reverse es) where
  findEdges t x = [a:b | b <- t, a <- edgeAddIndex x d]

-- find a common path in two cycles (which can be removed to form a
-- new cycle. Works for long (more than 2 vertices) cycles.
-- if there are more than one common paths (or even a path and a
-- point outside) then zero path is returned
cyclesCommonPath c1 c2 =
  let cps = [x | x <- paths, x `isInfixOf` c1', x `isInfixOf` c2'] where
        paths = permutations $ intersect c1 c2 
        c1' = c1 ++ c1
        c2' = c2 ++ c2
      res | null cps = []
          | length cps > 1 = error "More than one common path. Bug?"
          | otherwise = head cps
  in res

-- make a sum of two cycles
cyclesSum c1 [] = c1
cyclesSum [] c2 = c2
cyclesSum c1 c2 | null cp = []
                | length cp == 1 = []
                | otherwise = c1' ++ [b] ++ c2' where
  c1' = takeWhile (/= b) $ dropWhile (/= e) (c1 ++ c1)
  c2' = tail $ takeWhile (/= b) $ dropWhile (/= e) (c2 ++ c2)
  b = head cp
  e = last cp
  cp = cyclesCommonPath c1 c2

-- find a set of independent cycles (works for long cycles)
cyclesBasis' l = res where
  (res,_) = foldl addCycle ([],[[]]) l where
    addCycle (b,cs) c | not $ null $ intersectBy cycleEq cs [c] = (b,cs)
                      | otherwise =
      let cs' = [cyclesSum x c | x <- cs, not $ null $ cyclesSum x c] ++
                [cyclesSum x (reverse c) | x <- cs, not $ null $ cyclesSum x (reverse c)] ++
                cs
          cs'' = nubBy cycleEq cs'
      in (c:b,cs'')

-- now build the basis cycles for the diagram. 1-loops are always independent
-- so we can take them as is. Then we take as much independent short
-- cycles as we can. After that we need to add independent set of
-- long cycles (not including ones that has the same vertices but
-- different edges).
cyclesBasis d = (cyclesLoops d) ++ (cyclesShort' d) ++ longCs where
  longCs = zip cs (map makeIndexRepr cs) where
    cs = cyclesBasis' $ cyclesLong' d
    makeIndexRepr = head . (flip edgesToIndexed d) . cycleToEdges -- take any possible path

-- count number of loops (independent cycles, not 1-loops!!) in diagram
nrLoops :: Diagram -> Int
nrLoops = length . cyclesBasis

-- test if cycle lies inside subgraph
-- we just need to test if all vertices of cycle belong to subgraph
cycleLiesIn c d = null $ (fst c) \\ nodes (delNode 0 d)

-- test if cycle intersects (has common edge) with subgraph
cycleIntersects c d =
  not $ null $ intersect (cycleToEdges $ fst c) (edges d)   

-- internal cycles for subgraph (the ones that lie completely inside) 
subgraphInternalCycles cs d =
  [x | x <- cs, x `cycleLiesIn` d]

-- external cycles for subgraph (the ones that go through it)
subgraphExternalCycles cs d =
  [x | x <- cs, x `cycleIntersects` d, not (x `cycleLiesIn` d)]
  
-- the same but return indices of external cycles in the argument list
subgraphExternalCycles' cs d =
  [i | (i,c) <- zip [0..] cs, c `cycleIntersects` d, not (c `cycleLiesIn` d)]
  
-- count number of cycles from given set that lie inside a subgraph
nrCyclesIn cs d = length $ subgraphInternalCycles cs d

-- reverse cycle in edge representation
cycleReverseE c = map (\((a,b),i) -> ((b,a),i)) c

-- test if cycle set covers all edges of the diagram
cyclesCoverGraph cs d = length (edges d') == length es where
  d' = delNode 0 d
  es = foldl1 union cs'
  cs' = map (\x -> x ++ cycleReverseE x) (snd $ unzip cs)
  
-- cycle set is considered acceptable iff each subgraph has no less
-- internal cycles than it's number of loops
cyclesAcceptable' cs ds = 
  all (\x -> nrCyclesIn cs x >= nrLoops x) ds
  
-- version that has theory and diagram as it's arguments instead
-- of explicit list of subgraphs
cyclesAcceptable th d cs = cyclesAcceptable' cs (signSubgraphs th d)

-- subsequences of length n
subseqsN s n = [x | x <- subsequences s, length x == n]

-- test edges for equality
edgeEq e1 e2@((a,b),i) = e1 == e2 || e1 == e2' where
  e2' = ((b,a),i)
  
-- penalty calculated for each subgraph
-- for each subgraph there is a penalty for a complex incoming moment
-- and a penalty proportional to a number of edges in subgraph
-- with incoming moment
penaltyForSubgraph cs d = pCmplIncM + pIncMLength where
  pCmplIncM = 10 * (nIncM - 1) where
    nIncM = length ecs
  ecs = subgraphExternalCycles cs d
  pIncMLength = 100 * nEdgsIncM where
    nEdgsIncM = length $ intersect es cyclesEdgs where
      es = concatMap (flip edgeAddIndex d) (edges d')
      cyclesEdgs = foldl1 (unionBy edgeEq) (snd $ unzip ecs)
      d' = delNode 0 d

-- all possible paths for external moment
allExtMomentPaths d = concatMap makePathsFromCycle ecs where
  ecs = nubBy cycleEq [x | x <- cyclesIn' d, length x > 2, 0 `elem` x]
  makePathsFromCycle c = zip (repeat c) paths where
    paths = edgesToIndexed (cycleToEdges c) d

-- find a set of cycles plus external moment path with a minimum penalty
-- cycle set should cover all edges in diagram and be acceptable
optimalCycles th d = snd $ minimumBy compareFst (zip pnlts css) where
  compareFst (x,_) (y,_) = x `compare` y
  css = [ext:cs | ext <- allExtMomentPaths d, cs <- possibleCss] where
    possibleCss = [cs | cs <- subseqsN (cycles d) (nrLoops d),
                   cs `cyclesCoverGraph` d, cyclesAcceptable th d cs]
  pnlts = map calcPenalty css where
    calcPenalty cs = sum $ map (penaltyForSubgraph cs) ds where
      ds = d:(signSubgraphs th d)  

-- Put moments on the diagram
diagramAddMoments' :: Diagram -> [([Node],[(Edge,Int)])] -> Diagram
diagramAddMoments' d cs = mkGraph (labNodes d) edgsWthMs where
  edgsWthMs = unionBy ((==) `on` (\(a,b,_) -> (a,b))) csEdgsM (labEdges d)
  csEdgsM = map (\(((a,b),i),m) -> (a,b,((DProp,addZeroMults m),[]))) csEdgsM'
  csEdgsM' = map (\l -> let (es,ms) = unzip l in (head es,M' ms)) csEdgsGrpd
  csEdgsGrpd = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) csEdgsTgd
  csEdgsTgd = concatMap tagCycle cs'
  cs' = zip [0..] csEdgs
  csEdgs = (snd . unzip) cs
  tagCycle (n,c) = zip c (repeat (Plus,n)) ++ zip (cycleReverseE c) (repeat (Minus,n))
  
-- version that uses theory to get optimal cycles
diagramAddMoments th d = diagramAddMoments' d (optimalCycles th d)

-- compute diagram divergence index
diagramDivIndex th d = (nrLoops d) * (spaceDimension th) +
  (sum $ map (\(_,INode ((t,_),_)) -> elementDivIndex th t) (labNodes d')) +
  (sum $ map (\(_,_,((t,_),_)) -> elementDivIndex th t) edgs) where
    d' = delNode 0 d
    edgs = [x | x <- labEdges d', let (a,b,_) = x in a <= b]

-- put a new 2-edge vertex into line
-- FIXME!! don't care about line tag for now
diagramAddVertex d e@(a,b) =
  (edgs,new,vert,edgs) & d' where
    edgs = [(prop,a),(prop,b)]
    d' = delEdge e $ delEdge (b,a) d
    new = noNodes d

-- Diagram with coefficient type
type CDiagram = (Diagram,Rational)

-- compute the partial derivative by mu square of the diagram
-- this effectively means sum of diagrams with vertex placed
-- onto one of the internal lines with inverted sign
dMuSquare :: CDiagram -> [CDiagram]
dMuSquare (d,c) =
  map (\e -> (diagramAddVertex d e,-c)) intEdgs where
  intEdgs = [(a,b) | (a,b) <- edges $ delNode 0 d, a <= b]

-- try to factorize the result by grouping diagrams with the same nickel
factorize cds =
  [(d,c) | (d,c) <- map sumCoefs grpdCds, c /= 0] where
    grpdCds = groupBy ((==) `on` nickelCDia) $ sortBy (compare `on` nickelCDia) cds
    nickelCDia = nickelDiagram . fst
    sumCoefs l = (fst $ head l, sum $ snd $ unzip l)
  
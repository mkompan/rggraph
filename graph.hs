import Data.Graph.Inductive
import Data.Graph.Analysis
import Data.Number.Symbolic
import Char
import Data.List
import qualified Data.Set as Set
import Control.Monad

type Modifier = [Char]
type Modified a = (a,[Modifier])

type DElemType = [Char]

type DElement = Modified (DElemType, Sym Double)

data DNode = ENode
           | INode DElement deriving (Show)
type DLine = DElement

type Diagram = Gr DNode DLine

dia_empty = ([],0,ENode,[]) & (empty :: Diagram)

prop :: DLine
prop = (("prop",0),[])

vert :: DNode
vert = INode (("vert",0),[])

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

-- biconnected subgraphs with desired number of tails
signSubgraphs :: Diagram -> [Int] -> [Diagram]
signSubgraphs d ts =
  let ts' = Set.fromList ts
  in [x | x <- allSubgraphs d, isBCD x, Set.member (tailsNr x) ts']
     
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
-- for problems b.-d. we will need a list of all edges
-- we are purely functional, so "edges d" are always the same
-- so we can use index of the element in this list to
-- distinguish edges connecting the same vertices
-- TODO: test it with 1-loops!!!
cycles :: Diagram -> [([Node],[Int])]
cycles d =
  let d' = delNode 0 d
      e = edges d'
      -- find and count 1-loops
      ledgs = [x | x <- e, fst x == snd x]
      lps = map (\l@((v,_):xs) -> (v,length $ l)) (group ledgs)
      lps' = concatMap f lps where
        f (v,n) = zip (repeat [v]) loopEdgs where
          loopEdgs = map (\x -> [x]) (take n (elemIndices (v,v) e))
      -- short loops (removing "backward" edges)
      s = filter ((>1) . length) $ group [x | x <- e, fst x < snd x]
      s' = concatMap f s where
        f x = zip (repeat c) [[e1,e2] | e1 <- x', e2 <- x', e1 < e2] where
          x' = elemIndices (head x) (edges d)
          c = [a,b]
          (a,b) = head x
  in lps' ++ s' ++ (cyclesLong d)

-- return "long" cycles (counting cycle with the same vertices only once
-- even if there are more than one edge)
cyclesLong' d =
  let d' = delNode 0 d
  in nubBy cycleEq [x | x <- cyclesIn' d', length x > 2]
     
-- and now add the missing "long" cycles (along with their respective
-- edge indices representation)
cyclesLong d = concatMap f (cyclesLong' d) where
  f x = zip (repeat x) (flip edgesToIndices d . cycleToEdges $ x)
      
-- convert cycle from vertex to edge representation
cycleToEdges c =
  let (res,_) = foldr (\x (l,y) -> ((x,y):l,x)) ([],head c) c
  in res

-- convert cycle from edges written as (v1,v2) to representation
-- using indices in "edges diagram" list (we may get more than one)
edgesToIndices es d = foldl findEdges [[]] (reverse es) where
  findEdges t x = [a:b | b <- t, a <- elemIndices x (edges d)]

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

main = do
  putStrLn "Hello, World!"


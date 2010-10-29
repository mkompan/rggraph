import Data.Graph.Inductive
import Data.Graph.Analysis
import Data.Number.Symbolic
import Char
import Data.List
import qualified Data.Set as Set

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
  let (res,_,_) = foldl (flip parseChar) ([],[],1) s
  in res


parseChar '-' (ctxs,edgs,nr) = ((edgs,nr,vert,edgs) : ctxs,[],nr+1)
parseChar 'e' (ctxs,edgs,nr) = (ctxs,(prop,0):edgs,nr)
parseChar c (ctxs,edgs,nr) =
  let n = digitToInt c
  in (ctxs,(prop,n+1):edgs,nr)


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
        addTail d_ (_,n) = insEdge (0,n,prop) $ insEdge (n,0,prop) d_
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
     

main = do
  putStrLn "Hello, World!"


module Result where

import Data.List

import Diagram
import Graph
import Theory.Phi3
import LinComb
import Angles

printDiagram (Diagram d) = do
  putStr $ show d
  putStrLn ""

printLC lc f = mapM_ printOne lc where
  printOne (e,c) = do
    putStrLn $ show c
    f e

printLCD lc = printLC lc printDiagram

expandD th str = printLCD $ runLC ds where
  ds = diagramPutDots th d >>= diagramExpandADiffs th
  d = buildDiagramStr str

printAS (a,s) = do
  putStrLn $ show a
  putStrLn $ s
  
printLCAS lc = printLC lc printAS

expandS th str =
  printLCAS $ runLC $ fmap (\d -> (diagramAOps th d,diagramSymbolize d n pairs)) ds where
    ds = diagramPutDots th d >>= diagramExpandADiffs th
    d = buildDiagramStr str
    n = nrLoops d
    pairs = diagramAllMomentPairs d'
    Diagram d' = fst $ head $ runLC ds

printJacobian th str = stringifyJ $ jacobian n pairs where
  n = nrLoops d
  pairs = diagramAllMomentPairs d'
  d = buildDiagramStr str
  d' = diagramAddMoments th d

expandS' th str =
  map (addAOps . addCoeff) $ runLC $ fmap (\d -> (diagramAOps th d,diagramSymbolize d n pairs)) ds where
    ds = diagramPutDots th d >>= diagramExpandADiffs th
    d = buildDiagramStr str
    n = nrLoops d
    pairs = diagramAllMomentPairs d'
    Diagram d' = fst $ head $ runLC ds

addCoeff ((x,s),c) = (x,show (fromRational c) ++ "*" ++ s)

strAOps ops = intercalate "*" $ filter (not . null) $ map strOneOp ops where
  strOneOp (_,0) = ""
  strOneOp (n,k) = "(1-a_" ++ show n ++ ")**" ++ show k

addAOps (ops,str) | null strOps = str
                  | otherwise = strOps ++ "*" ++ str where
                    strOps = strAOps ops

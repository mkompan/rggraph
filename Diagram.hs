module Diagram where

import Data.Graph.Inductive
import Control.Monad

import Graph hiding (dMuSquare)
import LinComb
import Theory
import Theory.Phi3
import Moment
import Angles

data DExp = Diagram Diagram
          | DElement DElement deriving (Show)
            
getDElement (DElement e) = e
actOnElement f = (fmap getDElement) . f . DElement

dMu2 :: DExp -> LinComb Rational DExp
dMu2 (Diagram d) = fmap Diagram $ graphMap mapChain (actOnElement dMu2) d
dMu2 (DElement ((DVertex,_),_)) = LC [] -- FIXME! I'd like to use 0 or mzero here
dMu2 (DElement (p@(DProp,_),mods)) = return $ DElement (p,"dMu2":mods)

dP :: DExp -> LinComb Rational DExp
dP (Diagram d) = fmap Diagram $ graphMap mapChain (actOnElement dP) d
dP (DElement ((t,m),mods)) | m `hasMoment` 0 = return $ DElement ((t,m),"dP":mods)
                           | otherwise = LC []

dAn :: Int -> DExp -> LinComb Rational DExp
dAn n (Diagram d) = fmap Diagram $ graphMap mapChain (actOnElement $ dAn n) d
dAn n (DElement ((t,m),mods)) | m `isStretchedBy` n = return $ DElement ((t,m),("dA_" ++ show n):mods)
                              | otherwise = LC []

zeroP :: DExp -> LinComb Rational DExp
zeroP (Diagram d) = fmap Diagram $ graphMap mapAll (actOnElement zeroP) d
zeroP (DElement ((t,m),mods)) | "dP" `elem` mods = return $ DElement ((t,m),"zeroP":mods)
                              | otherwise = return $ DElement ((t,m `removeMoment` 0),mods)

unStretch :: Int -> DExp -> LinComb Rational DExp
unStretch n (Diagram d) = fmap Diagram $ graphMap mapAll (actOnElement $ unStretch n) d
unStretch n (DElement ((t,m),mods)) = return $ DElement ((t,m `momentUnStretch` n),mods)

-- power for functions. TODO: Find correct place for me
power f 0 = id
power f n = foldl1 (.) $ replicate n f

-- and monadic version
powerM f 0 = return
powerM f n = foldl1 (>=>) $ replicate n f

diagramHandleExtMoment th d = return d >>= (powerM dP n) >>= zeroP where
  n = -diagramDivIndex th d'
  Diagram d' = d

diagramAllMomentPairs d = pairsMany ls where
  ls = map (filter (/=0)) ls' -- we will handle ext moment separately
  ls' = map ((\(_,x,_) -> x).unzip3.moments) ms
  ms = (map (\(_,INode ((_,m),_)) -> m) $ labNodes d') ++
       (map (\(_,_,((_,m),_)) -> m) $ labEdges d')
  d' = delNode 0 d

diagramSignSubgraphsDivIndices th d =
  zip [1..] (map (diagramDivIndex th) (signSubgraphs th d))

diagramUnStretchUnneeded th d = unStretchOp d where
  unStretchOp = foldl (>=>) return $ map unStretch idxs
  idxs = [x | (x,i) <- diagramSignSubgraphsDivIndices th d', i<0]
  Diagram d' = d

diagramAOps th d =
  [(x,i) | (x,i) <- diagramSignSubgraphsDivIndices th d', i>=0] where
    Diagram d' = d

diagramSymbolize d n pairs = graphSymbolize d' n pairs where
  Diagram d' = d

diagramPutDots th d =
  (diagramHandleExtMoment th $ Diagram d') >>= dMu2 >>= (diagramUnStretchUnneeded th) where
    n = nrLoops d
    pairs = diagramAllMomentPairs d'
    d' = diagramStretchMoments th $ diagramAddMoments th d

diagramExpandADiffs th d = aDiffs (diagramAOps th d) d where
  aDiffs [] = return
  aDiffs ops = foldl1 (>=>) $ map (\(n,k) -> powerM (dAn n) (k+1)) ops

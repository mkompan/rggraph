module Diagram where

import Graph hiding (dMuSquare)
import LinComb
import Theory
import Moment

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

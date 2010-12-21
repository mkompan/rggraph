module Diagram where

import Graph hiding (dMuSquare)
import LinComb
import Theory

data DExp = Diagram Diagram
          | DElement DElement deriving (Show)
            
getDElement (DElement e) = e
actOnElement f = (fmap getDElement) . f . DElement

dMu2 :: DExp -> LinComb Rational DExp
dMu2 (Diagram d) = fmap Diagram $ graphMap mapChain (actOnElement dMu2) d
dMu2 (DElement ((DVertex,_),_)) = LC [] -- FIXME! I'd like to use 0 or mzero here
dMu2 (DElement (p@(DProp,_),mods)) = return $ DElement (p,"dMu2":mods)
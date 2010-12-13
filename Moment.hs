module Moment where

import Data.List

data Sign = Plus | Minus

instance Show Sign where
  show Plus = "+"
  show Minus = "-"
  
type Multiplier = [Int]
newtype Moment' = M' [(Sign,Int)]
newtype Moment = M [(Sign,Int,Multiplier)]

emptyMoment' = M' []
emptyMoment = M []

showOneM 0 = "p"
showOneM i = "q_" ++ show i

showMoment' (M' cs) = concatMap showOne cs where
  showOne (s,c) = show s ++ showOneM c

instance Show Moment' where
  show = showMoment'
  
showMultiplier ms =
  concat $ intersperse "*" (map showOne ms) where
    showOne m = "a_" ++ show m
    
showMoment (M cs) = concatMap showOne cs where
  showOne (s,c,mult) = (show s) ++ multStr ++ starStr ++ (showOneM c) where
    starStr | null multStr = ""
            | otherwise = "*"
    multStr = showMultiplier mult 

instance Show Moment where
  show = showMoment
  
addZeroMults (M' m') = M $ map (\(s,c) -> (s,c,[])) m'

-- stretch the moment by a_n factor along given list of basic moments
momentStretch (M m) n l = M $ map stretchOne m where
  stretchOne (s,q,a) | q `elem` l = (s,q,n:a)
                     | otherwise  = (s,q,a)
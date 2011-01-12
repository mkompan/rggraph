module Moment where

import Data.List
import Data.Function (on)

import Angles

data Sign = Plus | Minus deriving (Eq)

instance Show Sign where
  show Plus = "+"
  show Minus = "-"
  
type Multiplier = [Int]
newtype Moment' = M' [(Sign,Int)] deriving (Eq)
newtype Moment = M {moments :: [(Sign,Int,Multiplier)]} deriving (Eq)

instance Num Moment where
  M a + M b = M (a ++ b)
  negate = negateMoment

negateMoment (M m) = M $ map negateOne m where
  negateOne (Plus,p,a) = (Minus,p,a)
  negateOne (Minus,p,a) = (Plus,p,a)

emptyMoment' = M' []
emptyMoment = M []

showOneM 0 = "p"
showOneM i = "q_" ++ show i

showMoment' (M' []) = "0"
showMoment' (M' cs) = concatMap showOne cs where
  showOne (s,c) = show s ++ showOneM c

instance Show Moment' where
  show = showMoment'
  
showMultiplier ms =
  concat $ intersperse "*" (map showOne ms) where
    showOne m = "a_" ++ show m
    
showMoment (M []) = "0"
showMoment (M cs) = concatMap showOne cs where
  showOne (s,c,mult) = (show s) ++ multStr ++ starStr ++ (showOneM c) where
    starStr | null multStr = ""
            | otherwise = "*"
    multStr = showMultiplier mult 

instance Show Moment where
  show = showMoment
  
addZeroMults (M' m') = M $ map (\(s,c) -> (s,c,[])) m'

hasMoment (M m) n = n `elem` ns where
  (_,ns,_) = unzip3 m

removeMoment (M m) n =
  M $ deleteBy ((==) `on` (\(_,x,_) -> x)) (Plus,0,[]) m

-- stretch the moment by a_n factor along given list of basic moments
momentStretch (M m) n l = M $ map stretchOne m where
  stretchOne (s,q,a) | q `elem` l = (s,q,n:a)
                     | otherwise  = (s,q,a)

isStretchedBy (M m) n = any (elem n) mults where
  (_,_,mults) = unzip3 m

momentUnStretch (M m) n = M $ map unStretchOne m where
  unStretchOne (s,q,a) = (s,q,delete n a)

stringifySquare (M []) n pairs = "0"
stringifySquare (M m) n pairs = intercalate "+" (sqs ++ ps) where
  sqs =  map makeSquare m where
    makeSquare (_,q,a) = "(" ++ (show $ M [(Plus,q,a)]) ++ ")**2"
  ps = map makePaired [x | x <- subsequences m, length x == 2] where
    makePaired [x@(s,q,a),x'@(s',q',a')] =
      "2*(" ++ show (M [x]) ++ ")*(" ++ show (M [x']) ++ ")*" ++ cosStr where
        cosStr = "(" ++ (stringifyCos $ cosP (makePair q q') n pairs) ++ ")"

module Angles where

import Data.List
import Data.Maybe
import Data.Function (on)

data (Ord a) => Pair a = Pair a a

makePair x y | x<y   = Pair x y
             | otherwise = Pair y x
makePairL [x,y] = makePair x y

getPair (Pair x y) = [x,y]

instance (Show a,Ord a) => Show (Pair a) where
  show (Pair x y) = show (x,y)
  
instance (Ord a,Eq a) => Eq (Pair a) where
  (Pair x1 y1) == (Pair x2 y2) =
    ((x1==x2) && (y1==y2)) || ((x1==y2) && (y1==x2))

pairs l = map makePairL [x | x <- subsequences l, length x == 2]

pairsMany ls = foldl1 union $ map pairs ls

layerPairs order pairs = reverse res where
  (res,_) = foldl filterPairsWith ([],pairs) order
  filterPairsWith (lps,ps) e = (lps',ps') where
    lps' = newLayer:lps
    (newLayer,ps') = partition ((elem e).getPair) ps

layerPairs' order = (convertLayers order) . (layerPairs order)

convertLayers order layers =
  map (\(x,ps) -> (x,[y | p <- ps, y <- getPair p, y/=x])) $ zip order layers
  
convertLayers' layers' = (order,layers) where
  (order,_) = unzip layers'
  layers = map (\(e,l) -> map (makePair e) l) layers'

closeLayers' ls = res where
  (res,_) = foldl closeLayer ([],[]) $ reverse ls where
    closeLayer (clsdLs,fromPrev) (n,ps) =
      ((n,ps'):clsdLs,fromCur) where
        ps' = union ps fromPrev
        fromCur | null ps' = []
                | otherwise = n:ps'

closeLayers order layers =
  snd $ convertLayers' $ closeLayers' $ convertLayers order layers

scoreOrder order pairs =
  sum $ map (\(x,y) -> x^p*y) $ zip [0..] $ map length layrs where
    layrs = layerPairs order pairs
    p = length order
    
anglesOrder order pairs =
  sum $ map length $ closeLayers order $ layerPairs order pairs

optimalOrder n pairs = (m,angleMin) where
  m = minimumBy (compare `on` (\(x,_,_) -> x)) scoredOrders
  scoredOrders = zip3 scores orders angles
  scores = map (flip scoreOrder pairs) orders
  angles = map (flip anglesOrder pairs) orders
  orders = permutations [1..n]
  angleMin = minimum angles

layer (Pair x y) order =
  (min `on` fromJust) (elemIndex x order) (elemIndex y order)
  
cosP p@(Pair x y) n pairs =
  (i,prjs) where
    Just i = lookup p thetas
    thetas = zip (concat clsdLayers) [1..]
    clsdLayers = closeLayers order $ layerPairs order pairs
    ((_,order,_),_) = optimalOrder n pairs
    prjs = map (\e -> (fromJust $ lookup (Pair e x) thetas,
                       fromJust $ lookup (Pair e y) thetas)) upperLs
    upperLs = take (layer p order) order

strCos i = "x_" ++ show i
strSin i = "y_" ++ show i

stringifyCos (i,prjs) =
  (concatMap strOnePrj prjs) ++ strCos i ++ closingBrcts where
    strOnePrj (x,y) = strCos x ++ "*" ++ strCos y ++ "+" ++
                      strSin x ++ "*" ++ strSin y ++ "*("
    closingBrcts = replicate (length prjs) ')'

-- Jacobian for coordinate transformation
jacobian n pairs = zip [1..] ls where
  ls = concatMap (\(n,l) -> replicate (length l) n) nmbrdLayers
  nmbrdLayers = zip [0..] clsdLayers
  clsdLayers = closeLayers order $ layerPairs order pairs
  ((_,order,_),_) = optimalOrder n pairs

stringifyJ [] = "1"
stringifyJ as = intercalate "*" $ map strOne as where
  strOne (a,n) = "y_" ++ show a ++ "**(d-" ++ show (n+2) ++ ")"

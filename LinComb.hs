module LinComb where

import Data.Map (toList,fromListWith)
infixl 7 .*

data LinComb b a = LC {runLC :: [(a,b)]} deriving (Show,Eq)

mapC f (LC l) = LC $ map (\(e,c) -> (e,f c)) l

instance Functor (LinComb b) where
  fmap f (LC l) = LC $ map (\(e,c) -> (f e,c)) l

a .* b = mapC (a*) b

instance Num b => Monad (LinComb b) where
  return x = LC [(x,1)]
  l >>= f = LC $ concatMap (\(e,c) -> runLC $ c .* (f e)) (runLC l)

instance (Eq a,Show a,Num b) => Num (LinComb b a) where
  LC a + LC b = LC $ (a ++ b)
  a - b = a + (-1) .* b
  _ * _ = error "Num is annoying"
  abs _ = error "Num is annoying"
  signum _ = error "Num is annoying"
  fromInteger a = if a==0 then LC [] else error "fromInteger can only take zero argument"

collect :: (Num b,Ord a) => LinComb b a -> LinComb b a
collect = LC . toList . fromListWith (+) . runLC
  
expand :: Num b => [LinComb b a] -> LinComb b [a]
expand ((LC []):_) = LC []
expand [x] = do
  a <- x
  return [a]
expand (x:xs) = do
  a <- x
  as <- expand xs
  return (a:as)

-- map function a -> LinComb a to every element in
-- the multiplication (presented as list) at the same
-- time
mapAll :: Num b => (a -> LinComb b c) -> [a] -> LinComb b [c]
mapAll f = expand . (map f)

-- map function to every element in the multiplication
-- using the chain rule (for diff-like functions)
mapChain :: (Eq a,Show a,Num b) => (a -> LinComb b a) -> [a] -> LinComb b [a]
mapChain f l = sum $ map (\(i,x) -> mapAll (actOn i) x) ls where
  actOn i (n,x) | i == n = f x
                | otherwise = return x
  ls = zip [1..n] $ repeat lWithNmbrs
  lWithNmbrs = zip [1..] l
  n = length l


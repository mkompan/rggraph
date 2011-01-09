import System

import Result
import Theory.Phi3

main = do
  c <- getArgs
  expandS phi3 $ head c

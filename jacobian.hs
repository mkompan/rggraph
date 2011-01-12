import System

import Result
import Theory.Phi3

main = do
  args <- getArgs
  putStrLn $ printJacobian phi3 $ head args

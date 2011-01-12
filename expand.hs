import System

import Result
import Theory.Phi3

main = do
  c <- getArgs
  mapM_ putStrLn $ expandS' phi3 $ head c

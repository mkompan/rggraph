module Theory where

data DElemType = DVertex
               | DProp deriving (Show,Eq)

data Theory = Th {
  tailsSignDiagram :: [Int],
  spaceDimension :: Int,
  elementDivIndex :: DElemType -> Int
  }
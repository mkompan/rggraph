module Theory where

data DElemType = DVertex
               | DProp deriving (Show)

data Theory = Th {
  tailsSignDiagram :: [Int],
  spaceDimension :: Int,
  elementDivIndex :: DElemType -> Int
  }
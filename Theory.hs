module Theory where

import Moment

data DElemType = DVertex
               | DProp deriving (Show,Eq)
type Modifier = [Char]
type Modified a = (a,[Modifier])

type DElement = Modified (DElemType, Moment)

data Theory = Th {
  tailsSignDiagram :: [Int],
  spaceDimension :: Int,
  elementDivIndex :: DElement -> Int
  }
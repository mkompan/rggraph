module Theory.Phi3 where

import Theory

phi3 = Th {
  tailsSignDiagram = [2,3],
  spaceDimension = 6,
  elementDivIndex = phi3DivIndex
  }

phi3DivIndex DVertex = 0
phi3DivIndex DProp = -2

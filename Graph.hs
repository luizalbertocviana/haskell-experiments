module Graph where

import Data.Word ( Word64 )
import Combinatorial (pairs, cartesian)

type Vertex = Word64
type Edge = (Vertex, Vertex)

data Graph = G Word64 (Edge -> Bool)

empty :: Word64 -> Graph
empty n = G n (const False)

complete :: Word64 -> Graph
complete n = G n (const True)

vertices :: Graph -> [Vertex]
vertices (G n _) = [1..n]

edges :: Graph -> [Edge]
edges (G n adj) = filter adj $ pairs [1..n]

hasEdge :: Graph -> Edge -> Bool
hasEdge (G _ adj) = adj

addEdge :: Graph -> Edge -> Graph
addEdge g@(G n adj) edge =
  if adj edge
  then g
  else G n adj' where
    adj' e = e == edge || adj e

removeEdge :: Graph -> Edge -> Graph
removeEdge g@(G n adj) edge =
  if adj edge
  then G n adj'
  else g where
    adj' e = e /= edge && adj e

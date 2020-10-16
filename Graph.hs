module Graph (Graph, empty, edgeless,
              addVertex, hasVertex, vertices,
              addEdge, neighbors, hasEdge) where

import Prelude (Int, (+), (-), Bool,
                (==), (<=))

import Bool
import Functions
import List
import Maybe
import qualified SeqTree as Seq

type Vertex = Int

data Graph = G Int (Seq.SeqTree [Vertex])

empty :: Graph
empty = G 0 Seq.empty

addVertex :: Graph -> Graph
addVertex (G n adj) = G (n + 1) (Seq.insert adj [])

edgeless :: Int -> Graph
edgeless n = cond (n <= 0) empty (addVertex g) where
    g = edgeless (n - 1)

hasVertex :: Graph -> Vertex -> Bool
hasVertex (G n _) v = and [1 <= v, v <= n]

vertices :: Graph -> [Vertex]
vertices (G n _) = take n (iterate (+1) 1)

addEdge :: Graph -> Vertex -> Vertex -> Graph
addEdge g@(G n adj) u v = cond verticesExist g' g where
    verticesExist = and [g `hasVertex` u, g `hasVertex` v]
    g' = G n adj'
    adj' = Seq.update adj u (v:)

neighbors :: Graph -> Vertex -> [Vertex]
neighbors (G _ adj) v = fromMaybe [] (Seq.lookup adj v)

hasEdge :: Graph -> Vertex -> Vertex -> Bool
hasEdge g u v = f (neighbors g u) where
    f = isJust . find (==v)

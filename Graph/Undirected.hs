module Graph.Undirected () where

import Data.Word (Word64)

import qualified Graph.Directed as D
import Graph.Directed (Digraph, Vertex)

newtype Graph = G Digraph

type Edge = (Vertex, Vertex)

adjust :: Edge -> Edge
adjust e@(u, v) =
  if u > v
  then (v, u)
  else e

empty :: Word64 -> Graph
empty n = G (D.empty n)

complete :: Word64 -> Graph
complete n = G $ D.removeBackwardArcs $ D.complete n

loopless :: Graph -> Graph
loopless (G digraph) = G (D.loopless digraph)

vertices :: Graph -> [Vertex]
vertices (G digraph) = D.vertices digraph

edges :: Graph -> [Edge]
edges (G digraph) = D.arcs digraph

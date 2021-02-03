module Graph.Undirected () where

import Data.Word (Word64)

import qualified Graph.Directed as D

newtype Graph = G D.Digraph

type Edge = (D.Vertex, D.Vertex)

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

vertices :: Graph -> [D.Vertex]
vertices (G digraph) = D.vertices digraph

edges :: Graph -> [Edge]
edges (G digraph) = D.arcs digraph

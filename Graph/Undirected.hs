module Graph.Undirected () where

import Prelude hiding (cycle)

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

hasEdge :: Graph -> Edge -> Bool
hasEdge (G digraph) = D.hasArc digraph . adjust

withAdjustment :: (D.Digraph -> Edge -> D.Digraph) -> Graph -> Edge -> Graph
withAdjustment f (G digraph) = G . f digraph . adjust

addEdge :: Graph -> Edge -> Graph
addEdge = withAdjustment D.addArc

removeEdge :: Graph -> Edge -> Graph
removeEdge = withAdjustment D.removeArc

filterEdges :: (Edge -> Bool) -> Graph -> Graph
filterEdges p (G digraph) = G $ D.filterArcs p digraph

incidentEdges :: Graph -> D.Vertex -> [Edge]
incidentEdges g = filter (hasEdge g) . zip (vertices g) . repeat

neighbors :: Graph -> D.Vertex -> [D.Vertex]
neighbors g = map fst . incidentEdges g

path :: Word64 -> Graph
path = G . D.path

matching :: Word64 -> Graph
matching = G . D.matching

cycle :: Word64 -> Graph
cycle n = addEdge (path n) (n - 1, 0)

union :: Graph -> Graph -> Graph
union (G d1) (G d2) = G $ D.union d1 d2

shift :: Word64 -> Graph -> Graph
shift k (G digraph) = G $ D.shift k digraph

shiftedUnion :: Graph -> Graph -> Graph
shiftedUnion (G d1) (G d2) = G $ D.shiftedUnion d1 d2

complement :: Graph -> Graph
complement (G digraph) = G $ D.removeBackwardArcs $ D.complement digraph

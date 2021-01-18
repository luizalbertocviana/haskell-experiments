module Digraph () where

import Data.Word ( Word64 )
import Combinatorial (cartesian)

import Prelude (Ord((>=), max), Bool(..), (<), (&&), const, (+), (-), filter, ($), (||), not, (==), (/=), zip, repeat, otherwise, map, fst, snd)

type Vertex = Word64
type Arc = (Vertex, Vertex)

data Digraph = D Word64 (Arc -> Bool)

verify :: Word64 -> Arc -> Bool
verify n (u, v) = u < n && v < n

empty :: Word64 -> Digraph
empty n = D n (const False)

complete :: Word64 -> Digraph
complete n = D n (verify n)

vertices :: Digraph -> [Vertex]
vertices (D n _) = [0..n - 1]

arcs :: Digraph -> [Arc]
arcs d@(D _ adj) = filter adj $ cartesian verts verts where
  verts = vertices d

hasArc :: Digraph -> Arc -> Bool
hasArc d@(D n adj) = adj

addArc :: Digraph -> Arc -> Digraph
addArc d@(D n adj) arc =
  if adj arc || not (verify n arc)
  then d
  else D n adj' where
    adj' e = e == arc || adj e

removeArc :: Digraph -> Arc -> Digraph
removeArc d@(D n adj) arc =
  if adj arc && verify n arc
  then D n adj'
  else d where
    adj' e = e /= arc && adj e

outArcs :: Digraph -> Vertex -> [Arc]
outArcs d@(D n adj) u
  | u < n = filter adj $ zip (repeat u) (vertices d)
  | otherwise = []

inArcs :: Digraph -> Vertex -> [Arc]
inArcs d@(D n adj) v
  | v < n = filter adj $ zip (vertices d) (repeat v)
  | otherwise = []

outNeighbors :: Digraph -> Vertex -> [Vertex]
outNeighbors d u = map snd (outArcs d u)

inNeighbors :: Digraph -> Vertex -> [Vertex]
inNeighbors d v = map fst (inArcs d v)

path :: Word64 -> Digraph
path n = D n adj where
  adj arc@(u, v) = verify n arc && v == u + 1

cycle :: Word64 -> Digraph
cycle n = D n adj where
  adj arc = arc == (n - 1, 0) || adjPath arc
  (D _ adjPath) = path n

union :: Digraph -> Digraph -> Digraph
union (D n1 adj1) (D n2 adj2) = D n adj where
  n = max n1 n2
  adj arc = adj1 arc || adj2 arc

shift :: Word64 -> Digraph -> Digraph
shift k (D n adj) = D (n + k) adj' where
  adj' (u, v) = u >= k && v >= k && adj (u - k, v - k)

shiftedUnion :: Digraph -> Digraph -> Digraph
shiftedUnion d1@(D n1 _) d2 = union d1 $ shift n1 d2

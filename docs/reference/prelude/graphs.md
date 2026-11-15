# Graph Algorithms

## Graph Functions

> Graph algorithms on integer-indexed nodes [0..n-1]. EXPERIMENTAL.

| Function | Description |
|----------|-------------|
| `graph.union-find(n, edges)` | Connected components via union-find.
  Given `n` nodes and `edges` as `[[i,j],...]`, return list of n component labels |
| `graph.topo-sort(n, edges)` | Topological ordering via Kahn's algorithm.
  Given `n` nodes and directed `edges` as `[[from,to],...]`, return node ordering.
  Partial result if cycle exists (check count(result) = n) |
| `graph.kruskal-edges(n, edges)` | Kruskal-order merge history.
  Process `edges` in order via union-find. Return flat list of
  `[edge-index, remaining-components, ...]` for each edge that merges
  two distinct components |

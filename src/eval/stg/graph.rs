//! Graph algorithm intrinsics
//!
//! Node indices are represented as f64 (the eucalypt number type), so
//! precision is limited to 2^53. This is fine for typical use but
//! would silently lose precision for very large graphs.

use std::convert::TryInto;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
    },
};

use super::{
    force::SeqNumList,
    support::{collect_num_list, machine_return_num_list},
    syntax::{
        dsl::{annotated_lambda, app_bif, force, lref},
        LambdaForm,
    },
};

/// Graph union-find: takes (flat_edges, n_list) where flat_edges is
/// [i0, j0, i1, j1, ...] and n_list is [n].
/// Returns a list of n component labels.
pub struct GraphUnionFind;

impl StgIntrinsic for GraphUnionFind {
    fn name(&self) -> &str {
        "GRAPH_UNION_FIND"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            2, // [flat_edges, n_list]
            force(
                SeqNumList.global(lref(0)),
                // [concrete_edges] [flat_edges, n_list]
                force(
                    SeqNumList.global(lref(2)),
                    // [concrete_n_list, concrete_edges] [flat_edges, n_list]
                    app_bif(bif_index, vec![lref(1), lref(0)]),
                ),
            ),
            annotation,
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let edges = collect_num_list(machine, view, args[0].clone())?;
        let n_list = collect_num_list(machine, view, args[1].clone())?;
        let n =
            n_list.first().copied().ok_or_else(|| {
                ExecutionError::Panic("graph.union-find: n list empty".to_string())
            })? as usize;

        // Union-Find with path compression and union by rank
        let mut parent: Vec<usize> = (0..n).collect();
        let mut rank: Vec<usize> = vec![0; n];

        fn find(parent: &mut [usize], mut x: usize) -> usize {
            while parent[x] != x {
                parent[x] = parent[parent[x]]; // path halving
                x = parent[x];
            }
            x
        }

        fn union(parent: &mut [usize], rank: &mut [usize], x: usize, y: usize) {
            let rx = find(parent, x);
            let ry = find(parent, y);
            if rx != ry {
                match rank[rx].cmp(&rank[ry]) {
                    std::cmp::Ordering::Less => parent[rx] = ry,
                    std::cmp::Ordering::Greater => parent[ry] = rx,
                    std::cmp::Ordering::Equal => {
                        parent[ry] = rx;
                        rank[rx] += 1;
                    }
                }
            }
        }

        // Process edge pairs [i0, j0, i1, j1, ...]
        for pair in edges.chunks(2) {
            if pair.len() == 2 {
                let i = pair[0] as usize;
                let j = pair[1] as usize;
                if i < n && j < n {
                    union(&mut parent, &mut rank, i, j);
                }
            }
        }

        // Return labels with full path compression
        let labels: Vec<f64> = (0..n).map(|i| find(&mut parent, i) as f64).collect();
        machine_return_num_list(machine, view, labels)
    }
}

impl CallGlobal2 for GraphUnionFind {}

/// Graph topological sort: takes (flat_edges, n_list) where flat_edges is
/// [from0, to0, from1, to1, ...] and n_list is [n].
/// Returns topological ordering via Kahn's algorithm.
/// Partial result if cycle exists (caller detects via count(result) < n).
pub struct GraphTopoSort;

impl StgIntrinsic for GraphTopoSort {
    fn name(&self) -> &str {
        "GRAPH_TOPO_SORT"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            2, // [flat_edges, n_list]
            force(
                SeqNumList.global(lref(0)),
                // [concrete_edges] [flat_edges, n_list]
                force(
                    SeqNumList.global(lref(2)),
                    // [concrete_n_list, concrete_edges] [flat_edges, n_list]
                    app_bif(bif_index, vec![lref(1), lref(0)]),
                ),
            ),
            annotation,
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let edges = collect_num_list(machine, view, args[0].clone())?;
        let n_list = collect_num_list(machine, view, args[1].clone())?;
        let n = n_list
            .first()
            .copied()
            .ok_or_else(|| ExecutionError::Panic("graph.topo-sort: n list empty".to_string()))?
            as usize;

        // Build adjacency list and in-degree counts
        let mut adj: Vec<Vec<usize>> = vec![vec![]; n];
        let mut in_degree: Vec<usize> = vec![0; n];

        for pair in edges.chunks(2) {
            if pair.len() == 2 {
                let from = pair[0] as usize;
                let to = pair[1] as usize;
                if from < n && to < n {
                    adj[from].push(to);
                    in_degree[to] += 1;
                }
            }
        }

        // Kahn's algorithm (BFS)
        let mut queue: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
        for (i, &deg) in in_degree.iter().enumerate() {
            if deg == 0 {
                queue.push_back(i);
            }
        }

        let mut order: Vec<f64> = Vec::with_capacity(n);
        while let Some(node) = queue.pop_front() {
            order.push(node as f64);
            for &next in &adj[node] {
                in_degree[next] -= 1;
                if in_degree[next] == 0 {
                    queue.push_back(next);
                }
            }
        }

        machine_return_num_list(machine, view, order)
    }
}

impl CallGlobal2 for GraphTopoSort {}

/// Kruskal-order edge processing: takes (flat_edges, n_list) where
/// flat_edges is [i0, j0, i1, j1, ...] and n_list is [n].
/// Processes edges in order via union-find. For each edge that merges
/// two distinct components, emits [edge_index, remaining_components]
/// as a flat list: [idx0, comp0, idx1, comp1, ...].
pub struct GraphKruskalEdges;

impl StgIntrinsic for GraphKruskalEdges {
    fn name(&self) -> &str {
        "GRAPH_KRUSKAL_EDGES"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            2, // [flat_edges, n_list]
            force(
                SeqNumList.global(lref(0)),
                // [concrete_edges] [flat_edges, n_list]
                force(
                    SeqNumList.global(lref(2)),
                    // [concrete_n_list, concrete_edges] [flat_edges, n_list]
                    app_bif(bif_index, vec![lref(1), lref(0)]),
                ),
            ),
            annotation,
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let edges = collect_num_list(machine, view, args[0].clone())?;
        let n_list = collect_num_list(machine, view, args[1].clone())?;
        let n =
            n_list.first().copied().ok_or_else(|| {
                ExecutionError::Panic("graph.kruskal-edges: n list empty".to_string())
            })? as usize;

        let mut parent: Vec<usize> = (0..n).collect();
        let mut rank: Vec<usize> = vec![0; n];
        let mut components = n;

        fn find(parent: &mut [usize], mut x: usize) -> usize {
            while parent[x] != x {
                parent[x] = parent[parent[x]];
                x = parent[x];
            }
            x
        }

        // Result: flat [edge_index, remaining_components, ...]
        let mut result: Vec<f64> = Vec::new();

        for (edge_idx, pair) in edges.chunks(2).enumerate() {
            if pair.len() == 2 {
                let i = pair[0] as usize;
                let j = pair[1] as usize;
                if i < n && j < n {
                    let ri = find(&mut parent, i);
                    let rj = find(&mut parent, j);
                    if ri != rj {
                        // Union by rank
                        match rank[ri].cmp(&rank[rj]) {
                            std::cmp::Ordering::Less => parent[ri] = rj,
                            std::cmp::Ordering::Greater => parent[rj] = ri,
                            std::cmp::Ordering::Equal => {
                                parent[rj] = ri;
                                rank[ri] += 1;
                            }
                        }
                        components -= 1;
                        result.push(edge_idx as f64);
                        result.push(components as f64);
                    }
                }
            }
        }

        machine_return_num_list(machine, view, result)
    }
}

impl CallGlobal2 for GraphKruskalEdges {}

//! Packing solver intrinsic for heptomino-like tiling puzzles
//!
//! Solves: given a set of shapes (each a list of [r,c] cells), rotatable
//! and flippable, and a rectangular grid, can all pieces be placed without
//! overlap? Gaps are allowed (packing, not exact cover).

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
    support::{collect_num_list, machine_return_bool},
    syntax::{
        dsl::{annotated_lambda, app_bif, force, lref},
        LambdaForm,
    },
};

/// A placement is a list of encoded grid positions (row * w + col).
type Placement = Vec<usize>;

/// Generate all 8 orientations of a shape, deduplicate, and return
/// sorted normalised cell lists.
fn all_orientations(cells: &[(i32, i32)]) -> Vec<Vec<(i32, i32)>> {
    fn normalise(cells: &[(i32, i32)]) -> Vec<(i32, i32)> {
        let min_r = cells.iter().map(|&(r, _)| r).min().unwrap_or(0);
        let min_c = cells.iter().map(|&(_, c)| c).min().unwrap_or(0);
        let mut out: Vec<(i32, i32)> = cells.iter().map(|&(r, c)| (r - min_r, c - min_c)).collect();
        out.sort();
        out
    }

    fn rotate(cells: &[(i32, i32)]) -> Vec<(i32, i32)> {
        cells.iter().map(|&(r, c)| (c, -r)).collect()
    }

    fn flip(cells: &[(i32, i32)]) -> Vec<(i32, i32)> {
        cells.iter().map(|&(r, c)| (r, -c)).collect()
    }

    let mut variants: Vec<Vec<(i32, i32)>> = Vec::new();
    let mut current = cells.to_vec();
    for _ in 0..4 {
        let n = normalise(&current);
        if !variants.contains(&n) {
            variants.push(n);
        }
        let f = normalise(&flip(&current));
        if !variants.contains(&f) {
            variants.push(f);
        }
        current = rotate(&current);
    }
    variants
}

/// Generate all valid placements for a shape variant in a w×h grid.
/// Each placement is a list of encoded positions.
fn placements_for_variant(
    variant: &[(i32, i32)],
    w: usize,
    h: usize,
) -> Vec<Placement> {
    let anchor = variant[0]; // first cell in row-major order
    let mut result = Vec::new();

    for r in 0..h as i32 {
        for c in 0..w as i32 {
            let dr = r - anchor.0;
            let dc = c - anchor.1;
            let mut valid = true;
            let mut encoded = Vec::with_capacity(variant.len());
            for &(cr, cc) in variant {
                let nr = cr + dr;
                let nc = cc + dc;
                if nr < 0 || nr >= h as i32 || nc < 0 || nc >= w as i32 {
                    valid = false;
                    break;
                }
                encoded.push(nr as usize * w + nc as usize);
            }
            if valid {
                result.push(encoded);
            }
        }
    }
    result
}

/// Backtracking packing solver using bitmask for occupied cells.
/// Returns true if all pieces can be placed.
fn solve_packing(
    w: usize,
    h: usize,
    shape_placements: &[Vec<Placement>], // per shape type: all valid placements
    counts: &[usize],
) -> bool {
    let total_cells = w * h;

    // For each shape type, group placements by anchor cell
    // anchor_placements[type_idx][anchor_pos] = list of placements anchored at pos
    let mut anchor_placements: Vec<Vec<Vec<&Placement>>> = Vec::new();
    for type_placements in shape_placements {
        let mut by_anchor: Vec<Vec<&Placement>> = vec![vec![]; total_cells];
        for p in type_placements {
            // The anchor is the minimum encoded position
            let anchor = *p.iter().min().unwrap();
            by_anchor[anchor].push(p);
        }
        anchor_placements.push(by_anchor);
    }

    let n_types = counts.len();
    let mut counts = counts.to_vec();
    let total_needed: usize = counts
        .iter()
        .zip(shape_placements.iter())
        .map(|(&c, sp)| {
            c * sp.first().map_or(0, |p| p.len())
        })
        .sum();

    // Use u128 bitmask for grids up to 128 cells
    assert!(total_cells <= 128, "Grid too large for bitmask solver");

    fn solve_inner(
        occupied: u128,
        total_cells: usize,
        n_free: usize,
        needed: usize,
        counts: &mut [usize],
        anchor_placements: &[Vec<Vec<&Placement>>],
        n_types: usize,
    ) -> bool {
        if needed == 0 {
            return true;
        }
        if needed > n_free {
            return false;
        }

        // Find first empty cell
        let mut pos = 0;
        while pos < total_cells {
            if occupied & (1u128 << pos) == 0 {
                break;
            }
            pos += 1;
        }
        if pos >= total_cells {
            return needed == 0;
        }

        // Try each piece type at this anchor position
        for t in 0..n_types {
            if counts[t] == 0 {
                continue;
            }
            let piece_size = anchor_placements[t]
                .iter()
                .flat_map(|ps| ps.iter())
                .next()
                .map_or(0, |p| p.len());

            for placement in &anchor_placements[t][pos] {
                // Check no overlap
                let mut placement_mask = 0u128;
                let mut ok = true;
                for &cell in placement.iter() {
                    let bit = 1u128 << cell;
                    if occupied & bit != 0 {
                        ok = false;
                        break;
                    }
                    placement_mask |= bit;
                }
                if !ok {
                    continue;
                }

                // Place the piece
                counts[t] -= 1;
                let result = solve_inner(
                    occupied | placement_mask,
                    total_cells,
                    n_free - piece_size,
                    needed - piece_size,
                    counts,
                    anchor_placements,
                    n_types,
                );
                counts[t] += 1;

                if result {
                    return true;
                }
            }
        }

        // Skip this cell (forced gap)
        solve_inner(
            occupied | (1u128 << pos),
            total_cells,
            n_free - 1,
            needed,
            counts,
            anchor_placements,
            n_types,
        )
    }

    solve_inner(
        0u128,
        total_cells,
        total_cells,
        total_needed,
        &mut counts,
        &anchor_placements,
        n_types,
    )
}

/// PACK_CHECK intrinsic: takes (shapes_flat, params) → bool
///
/// shapes_flat: [r0,c0, r1,c1, ..., -1, r0,c0, ...] shapes separated by -1
/// params: [w, h, count0, count1, ...]
///
/// Returns true if all pieces (with given counts) can be packed into the
/// w×h grid without overlap.
pub struct PackCheck;

impl StgIntrinsic for PackCheck {
    fn name(&self) -> &str {
        "PACK_CHECK"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            2, // [shapes_flat, params]
            force(
                SeqNumList.global(lref(0)),
                // [concrete_shapes] [shapes_flat, params]
                force(
                    SeqNumList.global(lref(2)),
                    // [concrete_params, concrete_shapes] [shapes_flat, params]
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
        let shapes_flat = collect_num_list(machine, view, args[0].clone())?;
        let params = collect_num_list(machine, view, args[1].clone())?;

        if params.len() < 2 {
            return Err(ExecutionError::Panic(
                "pack-check: params must have at least [w, h]".to_string(),
            ));
        }

        let w = params[0] as usize;
        let h = params[1] as usize;
        let counts: Vec<usize> = params[2..].iter().map(|&x| x as usize).collect();

        // Parse shapes from flat representation (separated by -1)
        let mut shapes: Vec<Vec<(i32, i32)>> = Vec::new();
        let mut current_shape: Vec<(i32, i32)> = Vec::new();
        let mut i = 0;
        while i < shapes_flat.len() {
            let v = shapes_flat[i];
            if v < 0.0 {
                if !current_shape.is_empty() {
                    shapes.push(current_shape.clone());
                    current_shape.clear();
                }
            } else if i + 1 < shapes_flat.len() {
                let r = shapes_flat[i] as i32;
                let c = shapes_flat[i + 1] as i32;
                current_shape.push((r, c));
                i += 1; // skip the column value
            }
            i += 1;
        }
        if !current_shape.is_empty() {
            shapes.push(current_shape);
        }

        if counts.len() != shapes.len() {
            return Err(ExecutionError::Panic(format!(
                "pack-check: {} shapes but {} counts",
                shapes.len(),
                counts.len()
            )));
        }

        // Generate all orientations and valid placements for each shape
        let shape_placements: Vec<Vec<Placement>> = shapes
            .iter()
            .map(|shape| {
                let orientations = all_orientations(shape);
                let mut all_placements = Vec::new();
                for variant in &orientations {
                    all_placements.extend(placements_for_variant(variant, w, h));
                }
                // Deduplicate placements (different orientations may produce the same placement)
                all_placements.sort();
                all_placements.dedup();
                all_placements
            })
            .collect();

        let result = solve_packing(w, h, &shape_placements, &counts);
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal2 for PackCheck {}

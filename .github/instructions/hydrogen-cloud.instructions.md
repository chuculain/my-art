---
description: "Use when working on the Hydrogen Cloud Quil sketch, especially for refactors, bug fixes, performance changes, or feature updates in src/my_art/hydrogen_cloud.clj."
applyTo: "src/my_art/hydrogen-cloud.clj"
---

# Hydrogen Cloud Agent Guide

This file provides the minimum context a GitHub Copilot coding agent needs to understand and safely edit the Hydrogen Cloud app.

## What This App Is

- A Quil sketch that visualizes hydrogen orbital probability density cross-sections.
- Namespace: `my-art.hydrogen-cloud`.
- Primary file: `src/my_art/hydrogen-cloud.clj`.
- It renders 4 panel views of the same orbital slice with different Y-axis rotations.

## Runtime Model

The sketch has a split compute/render model:

1. Heavy numeric work runs off the render thread via `future`.
2. Quil image creation (`q/create-image`, pixel writes) runs on the render thread only.
3. `update-state` checks whether the compute future is ready and, when ready, converts grids to images.

Do not move Quil image API calls into background computation.

## Core Flow

1. `setup` creates initial state and starts a background compute future.
2. `compute-grids` computes probability grids and a global max value (`pmax`).
3. `grids->imgs` maps numeric grids into ARGB images via `grid->pimage`.
4. `draw-state` renders title, controls, 4 panel images, legend, and compute overlay.
5. `handle-key-pressed` updates quantum counters and launches a new compute future.

## State Contract

Expected state keys:

- `:n :l :m` quantum numbers
- `:a0` Bohr radius scale (pixels)
- `:rmax` sampling radius derived from `n`
- `:grid-w :grid-h` panel grid resolution
- `:grids` vector of `double-array` values (one per panel)
- `:imgs` vector of Quil images (same panel order)
- `:pmax` global max across all panel grids
- `:computing?` boolean loading flag
- `:compute-future` background job handle while computing

If you add fields, preserve compatibility with existing setup/update/draw flow.

## Math and Domain Notes

- Radial part: `radial-wavefn` using associated Laguerre polynomials.
- Angular part: `angular-density` using associated Legendre polynomials.
- Combined density: `probability-density`.
- Valid quantum state rules:
  - `n > 0`
  - `0 <= l < n`
  - `|m| <= l`
- Current UX clamps `m` to `0..l` in counter helpers.

When changing formulas, keep normalization behavior and edge-case handling near `r = 0` intact unless intentionally redesigning physics.

## Performance and Concurrency Constraints

- `compute-panel-grid` is the hot path.
- Trig/scales are hoisted from inner loops; avoid reintroducing per-pixel expensive recomputation.
- Grid computation uses row chunking + `pmap`; preserve deterministic array indexing.
- `draw-state` should remain mostly rendering-only (no heavy recomputation).

## Rendering and UX Conventions

- 4 panel layouts come from `panel-views` (`:rot`, `:label`, `:col`, `:row`).
- Color mapping is in `heatmap-color` and used for both panels and legend.
- Intensity transform currently uses `sqrt(p/pmax)` before color mapping.
- Up/Down arrows step through quantum counters.

If changing key behavior, keep transitions valid via counter helpers and clamping.

## Known Diagnostics Quirk

- clj-kondo may report unresolved symbol on `q/defsketch` sketch identifiers in this repo.
- Treat this as benign unless runtime behavior fails.

## Safe Edit Checklist

Before finalizing edits, verify:

1. Sketch still starts and renders all 4 panels.
2. Up/Down key transitions remain valid and responsive.
3. No Quil image API calls were moved off the render thread.
4. `:computing?` overlay appears during recompute and clears when done.
5. Legend gradient remains consistent with panel color mapping.

## Optional Run Notes

- Dependencies are defined in `project.clj` (`org.clojure/clojure` and `quil`).
- Typical workflow is Leiningen + REPL/Calva to evaluate the sketch namespace.

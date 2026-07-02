# Options for local elastic hierarchy transitions

Builds a validated list of animation and behaviour settings for
[`build_elastic_transition()`](https://prigasg.github.io/dragmapr/reference/build_elastic_transition.md).
Defaults follow the user-tested values from the dragmapr transition
prototype: a 400 ms branch bloom, a dotted group-drag boundary around
the expanded group, and a stable parent skeleton.

## Usage

``` r
transition_options(
  mode = c("local_elastic", "local_insert"),
  duration_ms = 400,
  overshoot = 0,
  max_stretch = 0.35,
  n_frames = 30,
  preserve_skeleton = TRUE,
  global_relayout = FALSE,
  show_ghost = TRUE,
  show_trails = FALSE,
  reset_boundary = NULL,
  group_drag_frame = TRUE,
  boundary_behavior = c("drag", "none"),
  boundary_drag_threshold = 8,
  boundary_label = "Drag to",
  boundary_padding = 0.025,
  drag_boundary_with_group = TRUE
)
```

## Arguments

- mode:

  Transition style. `"local_elastic"` (default) blooms children from the
  parent centroid with elastic overshoot; `"local_insert"` is an alias
  kept for forward compatibility and currently behaves identically.

- duration_ms:

  Single positive number. Total animation duration in milliseconds.
  Defaults to `400`.

- overshoot:

  Single non-negative number controlling the elastic "snap" of the
  ease-out-back curve. `0` disables overshoot entirely; the default `0`
  disables bounce for a cleaner map transition.

- max_stretch:

  Single positive number. Upper cap on the per-feature stretch strength
  (movement distance relative to the layout diagonal). Defaults to
  `0.35`.

- n_frames:

  Single integer (\>= 2). Number of animation frames produced for
  static/`ggplot2` playback. Defaults to `30`.

- preserve_skeleton:

  Logical. Keep the parent layout stable while a branch expands. Must
  remain `TRUE`; included so the contract is explicit and
  machine-readable.

- global_relayout:

  Logical. Must be `FALSE`. Provided only so that attempts to opt into a
  Dagre-style full relayout fail with a clear explanation.

- show_ghost:

  Logical. Render the collapsed parent as a faint ghost behind its
  expanded children. Defaults to `TRUE`.

- show_trails:

  Logical. Render dotted movement trails from the parent centroid to
  each child's final position. Defaults to `FALSE`.

- reset_boundary:

  Deprecated logical alias for `group_drag_frame`.

- group_drag_frame:

  Logical. Generate the dotted group-drag boundary around each expanded
  group. Defaults to `TRUE`.

- boundary_behavior:

  Boundary mode. Use `"drag"` to allow moving the expanded group from
  the dotted frame, or `"none"` to show no draggable frame.

- boundary_drag_threshold:

  Pixel threshold before a dotted-frame pointer movement counts as a
  drag instead of a click.

- boundary_label:

  Single string used as the helper label for the dotted group frame.
  Defaults to `"Drag to"`, allowing the interactive helper to show
  labels such as `"Drag to Essex"`.

- boundary_padding:

  Single positive number. Boundary padding as a fraction of the child
  layout's bounding-box diagonal. Defaults to `0.025` (2.5 percent).

- drag_boundary_with_group:

  Logical. The group-drag boundary is part of the expanded group and
  must move with it when dragged; it is never a static overlay. Defaults
  to `TRUE`.

## Value

A named list of class `"dragmapr_transition_options"`.

## Details

The transition model is always *stable skeleton + local branch bloom*:
the parent layout is preserved and children expand locally from the
parent centroid. `global_relayout` exists only as an explicit guard
rail; setting it to `TRUE` is an error because recomputing the whole
layout on every expansion destroys the user's mental map.

## See also

[`build_elastic_transition()`](https://prigasg.github.io/dragmapr/reference/build_elastic_transition.md),
[`make_group_boundaries()`](https://prigasg.github.io/dragmapr/reference/make_group_boundaries.md).

## Examples

``` r
transition_options()
#> $mode
#> [1] "local_elastic"
#> 
#> $duration_ms
#> [1] 400
#> 
#> $overshoot
#> [1] 0
#> 
#> $max_stretch
#> [1] 0.35
#> 
#> $n_frames
#> [1] 30
#> 
#> $preserve_skeleton
#> [1] TRUE
#> 
#> $global_relayout
#> [1] FALSE
#> 
#> $show_ghost
#> [1] TRUE
#> 
#> $show_trails
#> [1] FALSE
#> 
#> $reset_boundary
#> [1] TRUE
#> 
#> $group_drag_frame
#> [1] TRUE
#> 
#> $boundary_behavior
#> [1] "drag"
#> 
#> $boundary_drag_threshold
#> [1] 8
#> 
#> $boundary_label
#> [1] "Drag to"
#> 
#> $boundary_padding
#> [1] 0.025
#> 
#> $drag_boundary_with_group
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "dragmapr_transition_options"
transition_options(duration_ms = 800, overshoot = 0)
#> $mode
#> [1] "local_elastic"
#> 
#> $duration_ms
#> [1] 800
#> 
#> $overshoot
#> [1] 0
#> 
#> $max_stretch
#> [1] 0.35
#> 
#> $n_frames
#> [1] 30
#> 
#> $preserve_skeleton
#> [1] TRUE
#> 
#> $global_relayout
#> [1] FALSE
#> 
#> $show_ghost
#> [1] TRUE
#> 
#> $show_trails
#> [1] FALSE
#> 
#> $reset_boundary
#> [1] TRUE
#> 
#> $group_drag_frame
#> [1] TRUE
#> 
#> $boundary_behavior
#> [1] "drag"
#> 
#> $boundary_drag_threshold
#> [1] 8
#> 
#> $boundary_label
#> [1] "Drag to"
#> 
#> $boundary_padding
#> [1] 0.025
#> 
#> $drag_boundary_with_group
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "dragmapr_transition_options"
try(transition_options(global_relayout = TRUE))
#> Error : Global relayout is not supported: recomputing the whole layout on every expansion breaks mental-map stability. Keep `global_relayout = FALSE`; dragmapr always uses a stable skeleton with local elastic insertion.
```

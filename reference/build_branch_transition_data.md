# Build leaf-flip transition data

Prepares an `sf` object and transition list for parent-to-child
leaf-flip bloom in
[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md).

## Usage

``` r
build_branch_transition_data(
  x,
  parent_col,
  child_col,
  expanded = NULL,
  dissolve = TRUE,
  max_child_groups = 600L,
  animation = c("branch_bloom", "leaf_flip"),
  duration_ms = 375,
  easing = c("cubic-out", "cubic-in-out", "linear"),
  show_parent_ghost = FALSE,
  parent_ghost_opacity = 0.18,
  leaf_flip_strength = 0.16,
  leaf_child_scale = 0.86,
  leaf_expand_duration_factor = 0.82,
  leaf_collapse_duration_factor = 0.58,
  boundary = TRUE,
  boundary_behavior = c("drag", "none"),
  boundary_drag_threshold = 8,
  boundary_label = "Drag to",
  parent_key_col = "..dragmapr_parent_key..",
  child_key_col = "..dragmapr_child_key..",
  shell_col = "..dragmapr_parent_shell.."
)
```

## Arguments

- x:

  An `sf` object.

- parent_col:

  Parent grouping column.

- child_col:

  Child grouping column.

- expanded:

  Optional parent values to start expanded.

- dissolve:

  Add dissolved parent shell features for collapsed parents.

- max_child_groups:

  Maximum number of child groups allowed.

- animation:

  Animation style. Use `"branch_bloom"` for the clean group bloom or
  `"leaf_flip"` for the temporary leaf-proxy flip.

- duration_ms:

  Animation duration in milliseconds.

- easing:

  Animation easing name.

- show_parent_ghost:

  Logical. Show a faint parent shell while expanded.

- parent_ghost_opacity:

  Parent ghost opacity when shown.

- leaf_flip_strength:

  Leaf proxy rotation strength.

- leaf_child_scale:

  Starting scale for child polygons in leaf mode.

- leaf_expand_duration_factor:

  Leaf expand speed multiplier.

- leaf_collapse_duration_factor:

  Leaf collapse speed multiplier.

- boundary:

  Logical. Add a dotted drag boundary for expanded groups.

- boundary_behavior:

  Boundary mode. Use `"drag"` or `"none"`.

- boundary_drag_threshold:

  Pixel threshold before a boundary drag counts.

- boundary_label:

  Text prefix for the dotted drag boundary.

- parent_key_col, child_key_col, shell_col:

  Internal column names to add.

## Value

A list with `sf`, `transition`, `parent_key_col`, `child_key_col`,
`shell_col`, and `validation`.

# Build parent and child labels for branch bloom

Creates the dual-label table understood by the branch-bloom browser
helper. Parent labels are safe for normal use. Child labels are optional
and mainly useful for testing because many child labels can make branch
animation feel busy or slow.

## Usage

``` r
make_branch_bloom_labels(
  x,
  parent_key_col = "..dragmapr_parent_key..",
  child_key_col = "..dragmapr_child_key..",
  shell_col = "..dragmapr_parent_shell..",
  parent_label_col = parent_key_col,
  child_label_col = child_key_col,
  show_parent_labels = TRUE,
  show_child_labels = FALSE,
  max_label_chars = 36,
  connector = FALSE,
  label_type = "label"
)
```

## Arguments

- x:

  An `sf` object prepared by
  [`build_branch_transition_data()`](https://prigasg.github.io/dragmapr/reference/build_branch_transition_data.md),
  or the `sf` element from that return value.

- parent_key_col, child_key_col, shell_col:

  Column names holding the parent key, child key, and parent-shell flag.

- parent_label_col, child_label_col:

  Optional columns used for label text. Defaults to the parent and child
  key columns.

- show_parent_labels, show_child_labels:

  Include parent or child labels.

- max_label_chars:

  Maximum number of characters before label text is shortened with
  `...`.

- connector, label_type:

  Values written to the returned label table.

## Value

A data frame suitable for the `labels` argument of
[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md).
Returns a zero-row label table when both label levels are disabled.

# Select a readable subset of label IDs

This helper is useful for interfaces that should keep all label state,
but show only a manageable subset by default. It returns label IDs only;
callers can pass the result to `label_values` in
[`drag_map_prototype()`](https://prigasg.github.io/dragmapr/reference/drag_map_prototype.md)
or
[`render_dragged_map()`](https://prigasg.github.io/dragmapr/reference/render_dragged_map.md).

## Usage

``` r
select_label_ids(labels, max_labels = 25, prefer = NULL)
```

## Arguments

- labels:

  A drag label table accepted by
  [`as_drag_labels()`](https://prigasg.github.io/dragmapr/reference/as_drag_labels.md).

- max_labels:

  Maximum number of label IDs to return.

- prefer:

  Optional label IDs to keep first, for example labels from an expanded
  branch or labels the user has edited.

## Value

A character vector of label IDs.

## Examples

``` r
labels <- as_drag_labels(data.frame(
  label_id = c("A", "B", "C"), region = c("A", "B", "C"),
  label = c("A", "B", "C"), x = 1:3, y = 1:3
))
select_label_ids(labels, max_labels = 2)
#> [1] "A" "B"
```

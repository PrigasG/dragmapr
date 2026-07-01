# Update a dragmapr widget in place

Update a dragmapr widget in place

## Usage

``` r
updateDragmapr(session, outputId, ...)
```

## Arguments

- session:

  Shiny session.

- outputId:

  Widget output id.

- ...:

  Live updates to apply. Display options use the R API, for example
  `show_origin_outlines` or `map_background`. The composition field
  `selected_feature` can also be updated; pass `NULL` or `""` to clear
  the current selection. Use `remove_features = c(...)` to remove one or
  more feature ids from the live widget, or `delete_selected = TRUE` to
  remove the current selection.

## Value

Invisibly returns the sent message.

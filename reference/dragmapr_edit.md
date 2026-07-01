# Open the interactive dragmapr editor

A friendly front-door to the native draggable editor and the composition
step of the state-first workflow: compute a layout, edit it, render the
edited state. It accepts the objects produced upstream – a projected
`sf`, an explodemap `grouped_exploded_map`, or a `dragmapr_layout` –
together with an optional
[`dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/dragmapr_state.md),
and returns a configured
[`dragmapr_widget()`](https://prigasg.github.io/dragmapr/reference/dragmapr_widget.md)
ready to embed in Shiny, R Markdown, or the viewer.

## Usage

``` r
dragmapr_edit(x, region_col = NULL, state = NULL, ...)
```

## Arguments

- x:

  A projected `sf` object, an explodemap `grouped_exploded_map`, or a
  `dragmapr_layout`. Layout objects supply their own draggable geometry
  and region column.

- region_col:

  Column defining draggable groups. Required when `x` is a raw `sf`;
  inferred from layout objects when omitted.

- state:

  Optional
  [`dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/dragmapr_state.md)
  giving the initial composition.

- ...:

  Further arguments passed to
  [`dragmapr_widget()`](https://prigasg.github.io/dragmapr/reference/dragmapr_widget.md),
  for example `label_col`, `display_options`, `width`, or `height`.

## Value

A `dragmapr` htmlwidget (the interactive editor).

## Details

For an explodemap layout, pass the composed state explicitly so the
editor opens on the exploded composition rather than the bare geometry:

    layout <- explodemap::explode_grouped(x, region_col = "region")
    state  <- explodemap::as_dragmapr_state(layout)
    dragmapr_edit(layout, state = state)

To capture edits back into a `dragmapr_state` (to persist, merge, or
re-render them), read the widget's state input in Shiny with
[`dragmapr_widget_state()`](https://prigasg.github.io/dragmapr/reference/dragmapr_widget_state.md):

    output$map <- renderDragmapr(dragmapr_edit(layout, state = state))
    observeEvent(input$map_state, {
      state <- dragmapr_widget_state(input$map_state)
    })

## See also

[`dragmapr_widget()`](https://prigasg.github.io/dragmapr/reference/dragmapr_widget.md),
[`dragmapr_widget_state()`](https://prigasg.github.io/dragmapr/reference/dragmapr_widget_state.md).

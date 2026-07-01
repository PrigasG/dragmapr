# Reconstruct a dragmapr state from a browser state event

The native widget reports edits to Shiny through an input named
`paste0(outputId, "_state")`. This function turns that value back into a
[`dragmapr_state()`](https://prigasg.github.io/dragmapr/reference/dragmapr_state.md)
so server code can persist, merge, or re-render the user's live
composition. It is the inbound half of the widget bridge that
[`dragmapr_widget()`](https://prigasg.github.io/dragmapr/reference/dragmapr_widget.md)
/
[`updateDragmapr()`](https://prigasg.github.io/dragmapr/reference/updateDragmapr.md)
form on the outbound side.

## Usage

``` r
dragmapr_widget_state(value)
```

## Arguments

- value:

  The list received from `input[[paste0(outputId, "_state")]]`. `NULL`
  (no event yet) returns `NULL`.

## Value

A `dragmapr_state`, or `NULL` if `value` is `NULL`.

## Details

The browser's monotonic `revision` becomes the state `version` unchanged
– the client owns the live counter, so the server records the edit
faithfully rather than bumping it again. The empty-string selection
sentinel is mapped back to `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
shiny::observeEvent(input$map_state, {
  state <- dragmapr_widget_state(input$map_state)
  write_dragmapr_state(state, "composition.json")
})
} # }
```

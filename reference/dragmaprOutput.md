# Shiny bindings for dragmapr widgets

Shiny bindings for dragmapr widgets

## Usage

``` r
dragmaprOutput(outputId, width = "100%", height = "650px")

renderDragmapr(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- outputId:

  Shiny output id.

- width, height:

  Output dimensions.

- expr:

  Expression returning
  [`dragmapr_widget()`](https://prigasg.github.io/dragmapr/reference/dragmapr_widget.md).

- env:

  Evaluation environment.

- quoted:

  Is `expr` quoted?

## Value

Shiny output/render functions.

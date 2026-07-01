# Build the Shiny iframe bridge JavaScript for a draggable helper

Returns the JavaScript needed to pass region and label offset state from
the draggable helper iframe back into Shiny inputs via `postMessage`,
and to poll the iframe until state arrives. The string is suitable for
wrapping in `tags$head(tags$script(HTML(...)))`.

## Usage

``` r
dragmapr_iframe_bridge(
  region_input = "region_csv",
  label_input = "label_csv",
  slow_poll_ms = 2000L,
  fast_poll_ms = 500L,
  allowed_origin = "same-origin",
  iframe_selector = "iframe"
)
```

## Arguments

- region_input:

  Name of the Shiny input that receives the region-offset CSV text.
  Defaults to `"region_csv"`.

- label_input:

  Name of the Shiny input that receives the label-offset CSV text.
  Defaults to `"label_csv"`.

- slow_poll_ms:

  Polling interval in milliseconds once initial state has been received.
  Defaults to `2000`.

- fast_poll_ms:

  Polling interval in milliseconds before initial state arrives.
  Defaults to `500`.

- allowed_origin:

  Origin allowed to send drag state messages. Use `"same-origin"` to
  accept the current Shiny app origin, `"*"` to accept any origin, or a
  specific origin such as `"https://example.com"`.

- iframe_selector:

  CSS selector used to find the drag-map helper iframe in the parent
  document. Defaults to `"iframe"` (first iframe found), but a more
  specific selector such as `"#my-helper-frame"` or
  `"iframe.studio-helper-frame"` is recommended when the page may
  contain other iframes (e.g. Shiny download handlers).

## Value

A character string of JavaScript ready for
`tags$head(tags$script(HTML(dragmapr_iframe_bridge())))`.

## Examples

``` r
# In a Shiny UI:
if(interactive()){
library(shiny)
ui <- fluidPage(
  tags$head(tags$script(HTML(
    dragmapr_iframe_bridge(iframe_selector = "#my-helper-frame")
  ))),
  uiOutput("helper")  # render tags$iframe(id = "my-helper-frame", ...)
)
}
```

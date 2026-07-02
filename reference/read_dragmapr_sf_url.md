# Download and read an sf object from a URL

Downloads a spatial file from `url` into a temporary directory and reads
it with
[`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html).
Supported direct formats are `.geojson`, `.json`, and `.gpkg`. A `.zip`
URL is extracted first and the first supported file inside is read. For
ambiguous extensions the function assumes a zip archive.

## Usage

``` r
read_dragmapr_sf_url(url, timeout = 60)
```

## Arguments

- url:

  A non-empty character string pointing to a downloadable spatial file.

- timeout:

  Download timeout in seconds. Defaults to `60`.

## Value

An `sf` object.

## Examples

``` r
if(interactive()){
x <- read_dragmapr_sf_url("https://example.com/regions.geojson")
}
```

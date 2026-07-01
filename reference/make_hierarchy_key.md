# Build composite group keys from multiple columns

Creates a character vector of composite group identifiers by combining
values from two or more columns of a data frame or `sf` object. The
result is suitable as the `region` column of an offset data frame, and
is the key building block for parent-to-child layout inheritance in
hierarchical spatial datasets where child names repeat across parents.

## Usage

``` r
make_hierarchy_key(x, cols)
```

## Arguments

- x:

  A data frame or `sf` object.

- cols:

  A character vector of column names to combine into a composite key. At
  least one column is required. Columns must exist in `x`.

## Value

A character vector of length `nrow(x)`.

## Details

When `length(cols) == 1`, the values of that column are returned
directly (after whitespace trimming and NA replacement). When
`length(cols) > 1`, values are joined as
`"col1=val1 | col2=val2 | ..."`.

## See also

[`inherit_layout()`](https://prigasg.github.io/dragmapr/reference/inherit_layout.md)
to propagate parent offsets to child groups,
[`create_layout_snapshot()`](https://prigasg.github.io/dragmapr/reference/create_layout_snapshot.md)
to capture a layout for later restoration.

## Examples

``` r
df <- data.frame(
  county = c("Essex", "Essex", "Morris"),
  mun    = c("Fairfield", "Caldwell", "Fairfield")
)
# Single column: values returned as-is
make_hierarchy_key(df, "county")
#> [1] "Essex"  "Essex"  "Morris"

# Two columns: composite key disambiguates repeated child names
make_hierarchy_key(df, c("county", "mun"))
#> [1] "county=Essex | mun=Fairfield"  "county=Essex | mun=Caldwell"  
#> [3] "county=Morris | mun=Fairfield"
```

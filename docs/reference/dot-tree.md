# Tree printing method for list objects. This is an interactive function.

Tree printing method for list objects. This is an interactive function.

## Usage

``` r
.tree(x, max_levels = 6, ..., verbose = TRUE)
```

## Arguments

- x:

  A list

- max_levels:

  The maximum number of levels to show

- ...:

  Additional arguments:

  - `max_width` the number of items horizontally to show before
    truncating.

  - `max_length` the number of items vertically to show before
    truncating.

  - others are passed to `format(...)`

- verbose:

  print output to the console (the default)

## Value

The hierarchy as a string, called for side effects

# Delete all cached `SurvStat` requests

This function is only intended to be used interactively. The cache can
be controlled with
[`set_cache_settings()`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/set_cache_settings.md)

## Usage

``` r
cache_clear(confirm = utils::askYesNo("Are you sure?"))
```

## Arguments

- confirm:

  can be set to TRUE to make function non interactive.

## Value

nothing. called for side effects

## Examples

``` r
#' # Disable the cache for CRAN
set_cache_settings(active = FALSE)

cache_clear(confirm = interactive())
```

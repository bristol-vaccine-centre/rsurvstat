# Delete all cached `SurvStat` requests

The cache can be controlled with
[`set_cache_settings()`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/set_cache_settings.md)

## Usage

``` r
cache_clear()
```

## Value

nothing. called for side effects

## Examples

``` r
if (interactive()) cache_clear()
```

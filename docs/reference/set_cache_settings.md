# Set options for the `rsurvstat` cache

By default successful requests to `SurvStat` are cached for 7 days to
prevent repeated querying of the service. This is stored in the usual R
package cache location by default (e.g. `"~/.cache/rsurvstat"` on mac /
linux). Caching can be switched off altogether.

## Usage

``` r
set_cache_settings(..., active = NULL, dir = NULL, stale = NULL)
```

## Arguments

- ...:

  you can also submit the settings as a named list.

- active:

  boolean (optional), set to FALSE to disable caching

- dir:

  file path (optional), the location of the cache

- stale:

  numeric (optional), the number of days before a cached item is
  considered out of daye

## Value

the old cache settings as a list

## Examples

``` r
old_settings = set_cache_settings(active = FALSE)
set_cache_settings(old_settings)
```

# The `NutsKey71Map` dataset

This matches the `NutsKey71` dimension in `SurvStat`. This is the 38
`NUTS2` level administrative regions in Germany.

## Usage

``` r
data(NutsKey71Map)
```

## Format

A `sf` dataframe containing the following columns:

- `Id` - the full `SurvStat` identifier for this region (includes
  hierarchical information)

- `ComponentId` - the id of the most granular geographical unit (which
  can be used to link out to other data sets)

- `HierarchyId` - the id of the geographical unit type

- `Name` - the name of the region

38 rows

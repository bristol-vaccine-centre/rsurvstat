# The `CountyKey71Map` dataset

This matches the `CountyKey71` dimension in `SurvStat`. This is the 400
`Stadtkreis` and `Landkreise` administrative regions in Germany, plus 12
Berlin boroughs (`Bezirke`) which replace the Berlin `Kriese` (Id:
`11000`). The boroughs have sequential `Id`s from `[11001]` to `[11012]`

## Usage

``` r
data(CountyKey71Map)
```

## Format

A `sf` dataframe containing the following columns:

- `Id` - the full `SurvStat` identifier for this region (includes
  hierarchical information)

- `ComponentId` - the id of the most granular geographical unit (which
  can be used to link out to other data sets)

- `HierarchyId` - the id of the geographical unit type

- `Name` - the name of the region

Any grouping allowed.

411 rows

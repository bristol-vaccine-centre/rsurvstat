# The `FedStateKey71Map` dataset.

This matches the `FedStateKey71` dimension in `SurvStat`. This is the 16
federal states in Germany.

## Usage

``` r
data(FedStateKey71Map)
```

## Format

A `sf` dataframe containing the following columns:

- `Id` - the full `SurvStat` identifier for this region (includes
  hierarchical information)

- `ComponentId` - the id of the most granular geographical unit (which
  can be used to link out to other data sets)

- `HierarchyId` - the id of the geographical unit type

- `Name` - the name of the region

16 rows

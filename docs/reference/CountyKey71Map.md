# The CountyKey71Map dataset

This matches the `CountyKey71` dimension in SurvStat. This is the 400
"Stadtkreis" or "Landkreise" administrative regions in Germany, plus 12
Berlin boroughs ("Bezirke") which replace the Berlin Kriese (Id:
`11000`). The boroughs have sequential `Id`s from `[11001]` to `[11012]`

## Usage

``` r
data(CountyKey71Map)
```

## Format

A `sf` dataframe containing the following columns:

- Id - the full hierarchical value Id column

- ComponentId - the ComponentId column

- HierarchyId - the Id of the Hierarchy that this value applies to

- Name - A description of the item

Any grouping allowed.

411 rows

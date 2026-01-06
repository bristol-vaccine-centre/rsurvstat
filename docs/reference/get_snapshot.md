# Retrieve data from the `SurvStat` web service relating to a single time period.

This function gets a snapshot of disease count or incidence data from
the Robert Koch Institute `SurvStat` web service, based on either whole
epidemiological season or an individual week within a season. Seasons
are whole years starting either at the beginning of the calendar year,
at week 27 or at week 40.

## Usage

``` r
get_snapshot(
  disease = NULL,
  measure = c("Count", "Incidence"),
  ...,
  season,
  season_week = NULL,
  season_start = 1,
  age_group = NULL,
  age_range = c(0, Inf),
  disease_subtype = FALSE,
  geography = NULL,
  .progress = TRUE
)
```

## Arguments

- disease:

  the disease of interest as a `SurvStat` key, see
  [`rsurvstat::diseases`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/diseases.md)
  for a current list of these. This is technically optional, and if
  omitted the counts of all diseases will be returned. Keys are the same
  as the options in the `SurvStat` user interface found
  [here](https://survstat.rki.de/Content/Query/Main.aspx#CreateQuery).
  `IfSG` and `state` variants of diseases are counts that are reported
  directly to the Robert Koch Institute or indirectly via state
  departments.

- measure:

  one of `"Count"` (default) or `"Incidence"` per 100,000 per week or
  year depending on the context.

- ...:

  not used, must be empty.

- season:

  the start year of the season in which the snapshot is taken

- season_week:

  the start week within the season of the snapshot. If missing then the
  whole season is used

- season_start:

  the week of the calendar year in which the season starts this can be
  one of `1`, `27` or `40`.

- age_group:

  (optional) the age group of interest as a `SurvStat` key, see
  [`rsurvstat::age_groups`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/age_groups.md)
  for a list of valid options.

- age_range:

  (optional) a length 2 vector with the minimum and maximum ages to
  consider

- disease_subtype:

  if `TRUE` the returned count will be broken down by disease or
  pathogen subtype (assuming `disease` was provided).

- geography:

  (optional) a geographical breakdown. This can be given as a character
  where it must be one of `state`, `nuts`, or `county` specifying the 16
  region `FedStateKey71Map`, 38 region `NutsKey71Map`, or 411 region
  `CountyKey71Map` data respectively. Alternatively it can be given as a
  as a `sf` dataframe, subsetting one of these maps, in which case only
  that subset of regions will be returned.

- .progress:

  by default a progress bar is shown, which may be important if many
  downloads are needed to fulfil the request. It can be disabled by
  setting this to `FALSE` here.

## Value

a data frame with at least `year` (the start of the epidemiological
season) and `start_week` (the calendar week in which the epidemiological
season starts), and one of `count` or `incidence` columns. Most likely
it will also have `disease_name` and `disease_code` columns, and some of
`age_name`, `age_code`, `age_low`, `age_high`, `geo_code`, `geo_name`,
`disease_subtype_code`, `disease_subtype_name` depending on options.

## Details

The snapshot can be stratified by any combination of age, geography,
disease, disease subtype. Queries to `SurvStat` are cached and paged,
but obviously multidimensional extracts have the potential to need a lot
of downloading.

## Examples

``` r
get_snapshot(
  diseases$`COVID-19`,
  measure = "Count",
  season = 2024,
  age_group = age_groups$children_coarse
)
#> # A tibble: 11 × 9
#>     count age_name age_code    year start_week disease_name disease_code age_low
#>     <dbl> <chr>    <chr>      <dbl>      <dbl> <chr>        <chr>          <dbl>
#>  1  19636 0–14     [AlterPer…  2024          1 COVID-19     [KategorieN…       0
#>  2   5836 15–19    [AlterPer…  2024          1 COVID-19     [KategorieN…      15
#>  3   8463 20–24    [AlterPer…  2024          1 COVID-19     [KategorieN…      20
#>  4  10344 25–29    [AlterPer…  2024          1 COVID-19     [KategorieN…      25
#>  5  25431 30–39    [AlterPer…  2024          1 COVID-19     [KategorieN…      30
#>  6  25824 40–49    [AlterPer…  2024          1 COVID-19     [KategorieN…      40
#>  7  36957 50–59    [AlterPer…  2024          1 COVID-19     [KategorieN…      50
#>  8  45251 60–69    [AlterPer…  2024          1 COVID-19     [KategorieN…      60
#>  9  54930 70–79    [AlterPer…  2024          1 COVID-19     [KategorieN…      70
#> 10 104859 80+      [AlterPer…  2024          1 COVID-19     [KategorieN…      80
#> 11   1136 NA       [AlterPer…  2024          1 COVID-19     [KategorieN…      NA
#> # ℹ 1 more variable: age_high <dbl>

get_snapshot(
  diseases$`COVID-19`,
  measure = "Count",
  age_group = age_groups$children_coarse,
  season = 2024,
  geography = rsurvstat::FedStateKey71Map[1:10,]
)
#> # A tibble: 110 × 11
#>    count age_name age_code    year start_week disease_name disease_code geo_name
#>    <dbl> <chr>    <chr>      <dbl>      <dbl> <chr>        <chr>        <chr>   
#>  1  3287 0–14     [AlterPer…  2024          1 COVID-19     [KategorieN… Nordrhe…
#>  2   934 15–19    [AlterPer…  2024          1 COVID-19     [KategorieN… Nordrhe…
#>  3  1605 20–24    [AlterPer…  2024          1 COVID-19     [KategorieN… Nordrhe…
#>  4  2086 25–29    [AlterPer…  2024          1 COVID-19     [KategorieN… Nordrhe…
#>  5  4901 30–39    [AlterPer…  2024          1 COVID-19     [KategorieN… Nordrhe…
#>  6  4815 40–49    [AlterPer…  2024          1 COVID-19     [KategorieN… Nordrhe…
#>  7  7288 50–59    [AlterPer…  2024          1 COVID-19     [KategorieN… Nordrhe…
#>  8  8846 60–69    [AlterPer…  2024          1 COVID-19     [KategorieN… Nordrhe…
#>  9 10588 70–79    [AlterPer…  2024          1 COVID-19     [KategorieN… Nordrhe…
#> 10 19696 80+      [AlterPer…  2024          1 COVID-19     [KategorieN… Nordrhe…
#> # ℹ 100 more rows
#> # ℹ 3 more variables: geo_code <chr>, age_low <dbl>, age_high <dbl>
```

# Retrieve time series data from the RKI SurvStat api.

This function gets a weekly timeseries of disease count or incidence
data from the RKI `SurvStat` web API. The timeseries can be stratified
by any combination of age, geography, disease, disease subtype. Queries
to the API are cached and paged, but obviously multidimensional extracts
have the potential to need a lot of downloading.

## Usage

``` r
get_timeseries(
  disease = NULL,
  measure = c("Count", "Incidence"),
  ...,
  age_group = NULL,
  age_range = c(0, Inf),
  disease_subtype = FALSE,
  years = NULL,
  geography = NULL,
  trim_zeros = c("leading", "both", "none"),
  .progress = TRUE
)
```

## Arguments

- disease:

  the disease of interest, see
  [`rsurvstat::diseases`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/diseases.md).
  This is technically optional, and if omitted the counts of all
  diseases will be returned.

- measure:

  one of `"Count"` (default) or `"Incidence"`

- ...:

  not used, must be empty.

- age_group:

  (optional) the age group of interest, see
  [`rsurvstat::age_groups`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/age_groups.md)

- age_range:

  (optional) a length 2 vector with the minimum and maximum ages to
  consider

- disease_subtype:

  if `TRUE` the returned count will be broken down by disease or
  pathogen subtype (assuming `disease` was provided).

- years:

  (optional) a vector of years to limit the response to.

- geography:

  (optional) a geographical breakdown can be given as a character where
  it must be one of `state`, `nuts`, or `county` which align to the 16
  region `FedStateKey71Map`, 38 region `NutsKey71Map`, or 411 region
  `CountyKey71Map` data respectively. Alternatively it can be given as a
  subset of one of these maps, as a `sf` dataframe in which case only
  that subset of regions will be returned.

- trim_zeros:

  get rid of zero counts. Either "both" (from start and end), "leading"
  (from start only - the default) or "none".

- .progress:

  by default a progress bar is shown, which may be important if many
  downloads are needed to fulfil the request. It can be disabled here.

## Value

a data frame with at least `date` (weekly), and one of `count` or
`incidence` columns. Most likely it will also have `disease_name` and
`disease_code` columns, and some of `age_name`, `age_code`, `age_low`,
`age_high`, `geo_code`, `geo_name`, `disease_subtype_code`,
`disease_subtype_name` depending on options. The dataframe will be
grouped to make sure each group is a unique timeseries.

## Examples

``` r
get_timeseries(
  diseases$`COVID-19`,
  measure = "Count",
  age_group = age_groups$children_coarse
)
#> # A tibble: 3,376 × 8
#> # Groups:   age_name, age_code, disease_name, disease_code, age_low, age_high
#> #   [11]
#>    age_name age_code count date       disease_name disease_code age_low age_high
#>    <chr>    <chr>    <dbl> <date>     <chr>        <chr>          <dbl>    <dbl>
#>  1 0–14     [AlterP…     1 2020-02-03 COVID-19     [KategorieN…       0       15
#>  2 0–14     [AlterP…     2 2020-02-10 COVID-19     [KategorieN…       0       15
#>  3 0–14     [AlterP…     1 2020-02-17 COVID-19     [KategorieN…       0       15
#>  4 0–14     [AlterP…     4 2020-02-24 COVID-19     [KategorieN…       0       15
#>  5 0–14     [AlterP…    39 2020-03-02 COVID-19     [KategorieN…       0       15
#>  6 0–14     [AlterP…   197 2020-03-09 COVID-19     [KategorieN…       0       15
#>  7 0–14     [AlterP…   580 2020-03-16 COVID-19     [KategorieN…       0       15
#>  8 0–14     [AlterP…   910 2020-03-23 COVID-19     [KategorieN…       0       15
#>  9 0–14     [AlterP…  1018 2020-03-30 COVID-19     [KategorieN…       0       15
#> 10 0–14     [AlterP…   813 2020-04-06 COVID-19     [KategorieN…       0       15
#> # ℹ 3,366 more rows

get_timeseries(
  diseases$`COVID-19`,
  measure = "Count",
  age_group = age_groups$children_coarse,
  geography = rsurvstat::FedStateKey71Map[1:10,]
)
#> # A tibble: 33,258 × 10
#> # Groups:   age_name, age_code, disease_name, disease_code, geo_name, geo_code,
#> #   age_low, age_high [110]
#>    age_name age_code         count date       disease_name disease_code geo_name
#>    <chr>    <chr>            <dbl> <date>     <chr>        <chr>        <chr>   
#>  1 0–14     [AlterPerson80]…     1 2020-02-17 COVID-19     [KategorieN… Nordrhe…
#>  2 0–14     [AlterPerson80]…     4 2020-02-24 COVID-19     [KategorieN… Nordrhe…
#>  3 0–14     [AlterPerson80]…    12 2020-03-02 COVID-19     [KategorieN… Nordrhe…
#>  4 0–14     [AlterPerson80]…    23 2020-03-09 COVID-19     [KategorieN… Nordrhe…
#>  5 0–14     [AlterPerson80]…    92 2020-03-16 COVID-19     [KategorieN… Nordrhe…
#>  6 0–14     [AlterPerson80]…   122 2020-03-23 COVID-19     [KategorieN… Nordrhe…
#>  7 0–14     [AlterPerson80]…   175 2020-03-30 COVID-19     [KategorieN… Nordrhe…
#>  8 0–14     [AlterPerson80]…   151 2020-04-06 COVID-19     [KategorieN… Nordrhe…
#>  9 0–14     [AlterPerson80]…   113 2020-04-13 COVID-19     [KategorieN… Nordrhe…
#> 10 0–14     [AlterPerson80]…   150 2020-04-20 COVID-19     [KategorieN… Nordrhe…
#> # ℹ 33,248 more rows
#> # ℹ 3 more variables: geo_code <chr>, age_low <dbl>, age_high <dbl>

get_timeseries(
  measure = "Count",
  years = 2024
)
#> # A tibble: 3,861 × 5
#> # Groups:   disease_name, disease_code [80]
#>    disease_name  disease_code                             count date        year
#>    <chr>         <chr>                                    <dbl> <date>     <dbl>
#>  1 Acinetobacter [PathogenOut].[KategorieNz].[Krankheit …    16 2024-01-08  2024
#>  2 Acinetobacter [PathogenOut].[KategorieNz].[Krankheit …    22 2024-01-15  2024
#>  3 Acinetobacter [PathogenOut].[KategorieNz].[Krankheit …    26 2024-01-22  2024
#>  4 Acinetobacter [PathogenOut].[KategorieNz].[Krankheit …    16 2024-01-29  2024
#>  5 Acinetobacter [PathogenOut].[KategorieNz].[Krankheit …    14 2024-02-05  2024
#>  6 Acinetobacter [PathogenOut].[KategorieNz].[Krankheit …    14 2024-02-12  2024
#>  7 Acinetobacter [PathogenOut].[KategorieNz].[Krankheit …    16 2024-02-19  2024
#>  8 Acinetobacter [PathogenOut].[KategorieNz].[Krankheit …     9 2024-02-26  2024
#>  9 Acinetobacter [PathogenOut].[KategorieNz].[Krankheit …    14 2024-03-04  2024
#> 10 Acinetobacter [PathogenOut].[KategorieNz].[Krankheit …    11 2024-03-11  2024
#> # ℹ 3,851 more rows
```

# Retrieve time series data from the `SurvStat` web service.

This function gets a weekly timeseries of disease count or incidence
data from the Robert Koch Institute `SurvStat` web service. The
timeseries can be stratified by any combination of age, geography,
disease, disease subtype. Queries to `SurvStat` are cached and paged,
but obviously multidimensional extracts have the potential to need a lot
of downloading.

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

- years:

  (optional) a vector of years to limit the response to. This may be
  useful to limit the size of returned pages in the event the `SurvStat`
  service hits a data transfer limit.

- geography:

  (optional) a geographical breakdown. This can be given as a character
  where it must be one of `state`, `nuts`, or `county` specifying the 16
  region `FedStateKey71Map`, 38 region `NutsKey71Map`, or 411 region
  `CountyKey71Map` data respectively. Alternatively it can be given as a
  as a `sf` dataframe, subsetting one of these maps, in which case only
  that subset of regions will be returned.

- trim_zeros:

  get rid of zero counts. Either "both" (from start and end), "leading"
  (from start only - the default) or "none".

- .progress:

  by default a progress bar is shown, which may be important if many
  downloads are needed to fulfil the request. It can be disabled by
  setting this to `FALSE` here.

## Value

a data frame with at least `date` (weekly), and one of `count` or
`incidence` columns. Most likely it will also have `disease_name` and
`disease_code` columns, and some of `age_name`, `age_code`, `age_low`,
`age_high`, `geo_code`, `geo_name`, `disease_subtype_code`,
`disease_subtype_name` depending on options. The dataframe will be
grouped to make sure each group contains a single timeseries.

## Examples

``` r
# \donttest{
# Disable the cache for CRAN
rsurvstat::set_cache_settings(active=FALSE) 

# age stratified
get_timeseries(
  diseases$`COVID-19`,
  measure = "Count",
  age_group = age_groups$children_coarse
) %>% dplyr::glimpse()
#> Rows: 3,959
#> Columns: 8
#> Groups: age_name, age_code, disease_name, disease_code, age_low, age_high [11]
#> $ age_name     <chr> "0–14", "0–14", "0–14", "0–14", "0–14", "0–14", "0–14", "…
#> $ age_code     <chr> "[AlterPerson80].[AgeGroupName3].&[A00..14]", "[AlterPers…
#> $ date         <date> 2020-02-03, 2020-02-10, 2020-02-17, 2020-02-24, 2020-03-…
#> $ count        <dbl> 1, 2, 1, 4, 39, 197, 580, 910, 1018, 813, 575, 531, 401, …
#> $ disease_name <chr> "COVID-19", "COVID-19", "COVID-19", "COVID-19", "COVID-19…
#> $ disease_code <chr> "[KategorieNz].[Krankheit DE].&[COVID-19]", "[KategorieNz…
#> $ age_low      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ age_high     <dbl> 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 1…

# geographic
get_timeseries(
  diseases$`COVID-19`,
  measure = "Count",
  geography = "state"
) %>% dplyr::glimpse()
#> Rows: 5,731
#> Columns: 6
#> Groups: geo_name, geo_code, disease_name, disease_code [16]
#> $ geo_name     <chr> "Baden-Württemberg", "Baden-Württemberg", "Baden-Württemb…
#> $ geo_code     <chr> "[DeutschlandNodes].[Kreise71Web].[FedStateKey71].&[08]",…
#> $ date         <date> 2020-02-24, 2020-03-02, 2020-03-09, 2020-03-16, 2020-03-…
#> $ count        <dbl> 21, 213, 1337, 4467, 7238, 7523, 5305, 3210, 2438, 1229, …
#> $ disease_name <chr> "COVID-19", "COVID-19", "COVID-19", "COVID-19", "COVID-19…
#> $ disease_code <chr> "[KategorieNz].[Krankheit DE].&[COVID-19]", "[KategorieNz…

# disease stratified, subset of years:
get_timeseries(
  measure = "Count",
  years = 2024
) %>% dplyr::glimpse()
#> Rows: 3,835
#> Columns: 5
#> Groups: disease_name, disease_code [79]
#> $ disease_name <chr> "Acinetobacter", "Acinetobacter", "Acinetobacter", "Acine…
#> $ disease_code <chr> "[PathogenOut].[KategorieNz].[Krankheit DE].&[Acinetobact…
#> $ date         <date> 2024-01-08, 2024-01-15, 2024-01-22, 2024-01-29, 2024-02-…
#> $ count        <dbl> 16, 22, 26, 16, 14, 14, 16, 9, 14, 11, 18, 9, 13, 13, 14,…
#> $ year         <dbl> 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 202…
# }
```

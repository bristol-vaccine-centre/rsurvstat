# Infer and fit a population model from `SurvStat` output

`SurvStat` can be queried for count or incidence. From the combination
of these metrics queried across the whole range of disease notifications
for any given year we can infer a stratified population size, that
`SurvStat` is using to calculate it's incidence. This is simply modelled
with a local polynomial over time to allow us to fill in weekly
population denominators.

## Usage

``` r
fit_population(count_df, .progress = TRUE)

infer_population(
  age_group = NULL,
  geography = NULL,
  years = NULL,
  .progress = TRUE
)
```

## Arguments

- count_df:

  a dataframe from the output of
  [`get_timeseries()`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/get_timeseries.md)
  or
  [`get_snapshot()`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/get_snapshot.md)

- .progress:

  by default a progress bar is shown, which may be important if many
  downloads are needed to fulfil the request. It can be disabled by
  setting this to `FALSE` here.

- age_group:

  (optional) the age group of interest as a `SurvStat` key, see
  [`rsurvstat::age_groups`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/age_groups.md)
  for a list of valid options.

- geography:

  (optional) one of `"state"`, `"nuts"`, or `"county"` to define the
  resolution of the query. Does not accept a `sf` map or subset of
  (unlike
  [`get_timeseries()`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/get_timeseries.md)).

- years:

  (optional) a vector of years to limit the response to. This may be
  useful to limit the size of returned pages in the event the `SurvStat`
  service hits a data transfer limit.

## Value

the `count_df` dataframe with an additional `population` column

a dataframe with geography, age grouping, year and population columns

## Functions

- `infer_population()`: Query `SurvStat` for data to impute a population
  denominator

## Examples

``` r
# \donttest{

# Disable the cache for CRAN
rsurvstat::set_cache_settings(active = FALSE)

# snapshot:
get_snapshot(
  disease = diseases$`COVID-19`,
  geography = "state",
  season = 2024
) %>%
  fit_population() %>%
  dplyr::glimpse()
#> Rows: 16
#> Columns: 8
#> $ count        <dbl> 35131, 55907, 11789, 14524, 2034, 7606, 22941, 8962, 2856…
#> $ geo_name     <chr> "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "…
#> $ geo_code     <chr> "[DeutschlandNodes].[Kreise71Web].[FedStateKey71].&[08]",…
#> $ year         <dbl> 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 202…
#> $ start_week   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ disease_name <chr> "COVID-19", "COVID-19", "COVID-19", "COVID-19", "COVID-19…
#> $ disease_code <chr> "[KategorieNz].[Krankheit DE].&[COVID-19]", "[KategorieNz…
#> $ population   <dbl> 11245925.9, 13248951.2, 3685255.4, 2556742.8, 704883.8, 1…

# timeseries
# A weekly population estimate is inferred from the yearly data:
get_timeseries(
  diseases$`COVID-19`,
  measure = "Count",
  age_group = age_groups$children_coarse
) %>%
  fit_population() %>%
  dplyr::glimpse()
#> ■■■■■                             14% | ETA: 20s
#> ■■■■■■■■■■                        29% | ETA: 16s
#> ■■■■■■■■■■■■■■                    43% | ETA: 13s
#> ■■■■■■■■■■■■■■■■■■                57% | ETA: 10s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> Rows: 3,604
#> Columns: 9
#> Groups: age_name, age_code, disease_name, disease_code, age_low, age_high [10]
#> $ age_name     <chr> "0–14", "0–14", "0–14", "0–14", "0–14", "0–14", "0–14", "…
#> $ age_code     <chr> "[AlterPerson80].[AgeGroupName3].&[A00..14]", "[AlterPers…
#> $ disease_name <chr> "COVID-19", "COVID-19", "COVID-19", "COVID-19", "COVID-19…
#> $ disease_code <chr> "[KategorieNz].[Krankheit DE].&[COVID-19]", "[KategorieNz…
#> $ age_low      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ age_high     <dbl> 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 1…
#> $ date         <date> 2020-02-03, 2020-02-10, 2020-02-17, 2020-02-24, 2020-03-…
#> $ count        <dbl> 1, 2, 1, 4, 39, 197, 580, 910, 1018, 813, 575, 531, 401, …
#> $ population   <dbl> 11458731, 11459884, 11461012, 11462115, 11463193, 1146424…
# }
# \donttest{

# Disable the cache for CRAN
rsurvstat::set_cache_settings(active = FALSE)

infer_population(years = 2020:2025) %>% dplyr::glimpse()
#> ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  2s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> Rows: 6
#> Columns: 2
#> $ population <dbl> 83576971, 83577433, 84669255, 84358852, 83237083, 83154881
#> $ year       <int> 2025, 2024, 2023, 2022, 2021, 2020
# }
```

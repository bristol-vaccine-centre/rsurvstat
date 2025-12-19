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

- .progress:

  by default a progress bar is shown, which may be important if many
  downloads are needed to fulfil the request. It can be disabled here.

- age_group:

  (optional) the age group of interest, see
  [`rsurvstat::age_groups`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/age_groups.md)

- geography:

  (optional) one of `"state"`, `"nuts"`, or `"county"` to define the
  resolution of the query. Does not accept a `sf` map or subset of
  (unlike
  [`get_timeseries()`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/get_timeseries.md)).

- years:

  (optional) a vector of years to limit the response to.

## Value

the `count_df` dataframe with an additional `population` column

a dataframe with geography, age grouping, year and population columns

## Functions

- `infer_population()`: Query `SurvStat` for data to impute a population
  denominator

## Examples

``` r
get_timeseries(
  disease = diseases$`COVID-19`,
  geography = "state"
) %>%
fit_population() %>%
dplyr::glimpse()
#> Rows: 4,883
#> Columns: 7
#> Groups: geo_name, geo_code, disease_name, disease_code [16]
#> $ geo_name     <chr> "Baden-Württemberg", "Baden-Württemberg", "Baden-Württemb…
#> $ geo_code     <chr> "[DeutschlandNodes].[Kreise71Web].[FedStateKey71].&[08]",…
#> $ disease_name <chr> "COVID-19", "COVID-19", "COVID-19", "COVID-19", "COVID-19…
#> $ disease_code <chr> "[KategorieNz].[Krankheit DE].&[COVID-19]", "[KategorieNz…
#> $ date         <date> 2020-02-24, 2020-03-02, 2020-03-09, 2020-03-16, 2020-03-…
#> $ count        <dbl> 21, 213, 1337, 4467, 7238, 7523, 5305, 3210, 2438, 1229, …
#> $ population   <dbl> 11116513, 11115901, 11115274, 11114633, 11113977, 1111330…


infer_population(years=2020:2025) %>% dplyr::glimpse()
#> Rows: 6
#> Columns: 2
#> $ population <dbl> 83577373, 83577407, 84669475, 84358840, 83237155, 83154897
#> $ year       <int> 2025, 2024, 2023, 2022, 2021, 2020
```

# ISO 8601 weeks to date

ISO 8601 weeks to date

## Usage

``` r
date_from_iso(years, weeks)
```

## Arguments

- years:

  the ears of an ISO week date

- weeks:

  the week in the year of the ISO date

## Value

a vector of dates from the monday of the iso week

## Unit tests


    years = c(2001,2001,2004,2028)
    weeks = c(   1,  52,  53,  52)
    correct = c("2001-01-01","2001-12-24","2004-12-27","2028-12-25") 
    # format(correct, "

    testthat::expect_equal(
      date_from_iso(years, weeks),
      correct
    )

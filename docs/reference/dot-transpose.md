# Transform a nested dataframe to / from a row by row list

Data frames are column lists, which may have nested dataframes. This
function transforms a data frame to row based list with named sub lists
with one entry per dataframe column (a `row_list`). It alternative
converts a `row_list` back to a nested data frame

## Usage

``` r
.transpose(x, ..., .fix = ".")
```

## Arguments

- x:

  a `data.frame` or `row_list`

- ...:

  not used

- .fix:

  collapse or expand names in redundant multi-level `row_list`s. Either
  `FALSE` or a string to join or split the names of the multi-level list
  by

## Unit tests



    # create a test nested data frame:

    mtcars_nest = datasets::mtcars 
      dplyr::mutate(name = rownames(.)) 
      tidyr::nest(by_carb = -c(cyl,gear,carb)) 
      tidyr::nest(by_cyl_and_gear = -c(cyl,gear))

    mtcars_list = mtcars_nest 

    mtcars_nest2 = mtcars_list 

    testthat::expect_equal(mtcars_nest, mtcars_nest2)

# Convert a nested dataframe to a multilevel list

Convert a nested dataframe to a multilevel list

## Usage

``` r
.df_to_list_of_lists(df, ...)
```

## Arguments

- df:

  a nested dataframe

- ...:

  Named arguments passed on to
  [`.transpose`](https://bristol-vaccine-centre.github.io/rsurvstat/reference/dot-transpose.md)

  `x`

  :   a `data.frame` or `row_list`

  `.fix`

  :   collapse or expand names in redundant multi-level `row_list`s.
      Either `FALSE` or a string to join or split the names of the
      multi-level list by

  `...`

  :   not used

## Value

a list of lists

## Unit tests



    iris_list = .df_to_list_of_lists(datasets::iris)
    # TODO: iris_list has lost Petal.Length as it is interpreting Petal.Width as
    # nested item and it overwrites Petal.Length rather than merging with it.

    testthat::expect_equal(
      iris_list[[1]]$Species,
      iris$Species[[1]]
    )

    mtcars_nest = datasets::mtcars 
      dplyr::mutate(name = rownames(.)) 
      tidyr::nest(details = -c(cyl,gear))

    mtcars_list = mtcars_nest 

    mtcars_unnest = mtcars_list 

    testthat::expect_equal(
      mtcars_list[[1]]$details[[1]]$name,
      mtcars_nest$details[[1]]$name[[1]]
    )

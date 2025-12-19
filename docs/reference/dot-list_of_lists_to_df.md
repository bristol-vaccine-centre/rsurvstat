# Convert a multilevel list to a nested dataframe

Convert a multilevel list to a nested dataframe

## Usage

``` r
.list_of_lists_to_df(lst, ...)
```

## Arguments

- lst:

  a multilevel list

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

a dataframe with each sublist nested as a dataframe

## Unit tests


    iris_list = .df_to_list_of_lists(iris, .fix=FALSE)
    iris2 = .list_of_lists_to_df(iris_list, .fix=FALSE)

    testthat::expect_equal(datasets::iris, as.data.frame(iris2))

    mtcars_nest = datasets::mtcars 
      dplyr::mutate(name = rownames(.)) 
      tidyr::nest(details = -c(cyl,gear))

    mtcars_list = mtcars_nest 

    mtcars_nest2 = mtcars_list 

    testthat::expect_equal(
      mtcars_nest2$details[[2]],
      mtcars_nest$details[[2]]
    )

    # test unequal length vector column is mapped to list of vectors
    # and multiply named nests are treated as rows
    testlist = list(
       row = list(a=1:5, b="x"),
       row = list(a=2:4, b="y"),
       row = list(a=3, b="z")
    )
    testdf = testlist 
    testthat::expect_equal(testdf$b, c("x", "y", "z"))
    testthat::expect_equal(testdf$a[[2]], 2:4)

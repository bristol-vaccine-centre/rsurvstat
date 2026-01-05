# rsurvstat

Get case count data from the Robert Koch Institute `SurvStat` service.

## Installation

In general use `rsurvstat` is expected to be installed alongside the
`tidyverse` as `sf` set of packages. It is recommended to install these
first.

Binary packages of `rsurvstat` are available on CRAN and r-universe for
`macOS` and `Windows`. `rsurvstat` can be installed from source on
Linux.

You can install the released version of `rsurvstat` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rsurvstat")
```

### Alternative versions of `rsurvstat`

Early release versions are available on the `r-universe`. This will
typically be more up to date than CRAN.

``` r
# Enable repository from the bristol vaccine centre
options(repos = c(
  "bristol-vaccine-centre" = 'https://bristol-vaccine-centre.r-universe.dev/',
  CRAN = 'https://cloud.r-project.org'))
# Download and install rsurvstat in R
install.packages('rsurvstat')
```

The unstable development version is available from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bristol-vaccine-centre/rsurvstat")
```

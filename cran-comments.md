## Test environments

Github actions environments

* os: macOS-latest,   r: 'release'
* os: windows-latest, r: 'release'
* os: ubuntu-latest,   r: 'devel'
* os: ubuntu-latest,   r: 'release'
* os: ubuntu-latest,   r: 'oldrel-1'

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* This is a resubmission following feedback from CRAN on 9/1/2006.

## Other info

This is a API wrapper for the RKI `SurvStat` web service. The examples have been
picked balancing speed versus utility, but network issues may make some of them 
slow to run. Data dowload functions have been wrapped in `\donttest`

There is/was an erroneous NOTE due to spelling of "subtype" which is allowable
British English according to Collins: https://www.collinsdictionary.com/dictionary/english/subtype.
This is not replicated by `spelling::spell_check_package()`

Fixes from previous submission:

1) Web service URL added to description field in DESCRIPTION file

2) Missing return values fixed:
`.transpose()` - added return value to `Rd` file. This function is not exported 
  and documented with `@keywords internal`
`.tree()` - added return value to `Rd` file and removed function from package exports
  this is an internal debugging function documented with `@keywords internal`.

3) instances of `if(interactive())` & data download functions.
`get_timeseries()` - examples wrapped in `\donttest` due to data download / long running
`get_snapshot()` - examples wrapped in `\donttest` due to data download / long running
`fit_population()`  - examples wrapped in `\donttest` due to data download / long running
`infer_population()` - examples wrapped in `\donttest` due to data download / long running
`clear_cache()` - restructured function to make it explicit the function intended 
  to be used interactively, with parameter to confirm non-interactive use. 
  Interactive check removed from example.
  
4) You write information messages to the console that cannot be easily suppressed.
`R/import-standalone-df-list-df.R` - removed offending function from package 
  exports. It is an internal debugging function.
`R/survstat-loader.R` - restructured to use message()
`R/survstat-options.R` - checked only use of `cat()` is in S3 print method. Possibly
  this was flagged as a false positive?

5) Please always add all authors, contributors and copyright holders...
Added Bristol vaccine centre to description file as copyright holder role.


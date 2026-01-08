## Test environments

Github actions environments

* os: macOS-latest,   r: 'release'
* os: windows-latest, r: 'release'
* os: ubuntu-latest,   r: 'devel'
* os: ubuntu-latest,   r: 'release'
* os: ubuntu-latest,   r: 'oldrel-1'

## R CMD check results

0 errors | 0 warnings | 2 note

* This is a new release.

## Other info

This is a API wrapper for the RKI `SurvStat` web service. The examples have been
picked balancing speed versus utility, but network issues may make some of them 
slow to run.

There is/was an erroneous NOTE due to spelling of "subtype" which is allowable
british english according to collins: https://www.collinsdictionary.com/dictionary/english/subtype
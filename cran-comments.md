## Test environments

Github actions environments

* os: macOS-latest,   r: 'release'
* os: windows-latest, r: 'release'
* os: ubuntu-latest,   r: 'devel'
* os: ubuntu-latest,   r: 'release'
* os: ubuntu-latest,   r: 'oldrel-1'

## R CMD check results

0 errors | 0 warnings | 0 note

## Other info

* This is a minor bug fix submission to address the following issue raise by the 
  web checks service. The data set (a map) provided has been resampled to a 
  lower resolution:

Version: 0.1.2
Check: installed package size
Result: NOTE 
    installed size is  5.1Mb
    sub-directories of 1Mb or more:
      data   4.4Mb
Flavors: r-oldrel-macos-arm64, r-oldrel-macos-x86_64



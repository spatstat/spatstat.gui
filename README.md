# spatstat.gui

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/spatstat.gui)](http://cran.r-project.org/web/packages/spatstat.gui)
[![GitHub R package version](https://img.shields.io/github/r-package/v/spatstat/spatstat.gui)](https://github.com/spatstat/spatstat.gui)

This is an _extension_ of the `spatstat` package. 

It contains interactive graphics commands `iplot` and `istat` which were
originally part of the `spatstat` package, but have now been moved to
the new package `spatstat.gui`.

This GitHub repository holds the *development version* of
`spatstat.gui`. The development version is newer than the *official release*
of `spatstat.gui` on CRAN. 

## Installing the official release

For the most recent **official release** of 
`spatstat.gui`, see the [CRAN page](https://cran.r-project.org/web/packages/spatstat.gui). To install it, just type

```R
install.packages('spatstat.gui')
```

## Installing the development version

The easiest way to install the **development version** of `spatstat.gui` 
from github is through the `remotes` package:

```R
require(remotes)
install_github('spatstat/spatstat')
install_github('spatstat/spatstat.gui')
```

If you don't have `remotes` installed you should first run

```R
install.packages('remotes')
```

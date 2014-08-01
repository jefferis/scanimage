# scanimage
[![Build Status](https://travis-ci.org/jefferis/scanimage.svg)](https://travis-ci.org/jefferis/scanimage)

An R package to read and analyse image data acquired using ScanImage. This 
will typically be multiphoton imaging for neurobiological experiments.
See [scanimage.org](http://scanimage.org) for details of this software.

## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/) 
but you can install using devtools.

```r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("jefferis/scanimage")
```

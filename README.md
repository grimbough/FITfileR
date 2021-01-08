FITfileR
========

[![](https://img.shields.io/badge/dev%20version-0.1.1-blue.svg)](https://github.com/grimbough/FITfileR)
[![R CMD
check](https://github.com/grimbough/FITfileR/workflows/R-CMD-check/badge.svg)](https://github.com/grimbough/FITfileR/actions)
[![codecov](https://codecov.io/github/grimbough/FITfileR/branch/fit-class/graphs/badge.svg)](https://codecov.io/github/grimbough/FITfileR)

**FITfileR** is an R package to read FIT files produced by fitness
tracking devices like cycling computers or sports watches. The intention
for **FITfileR** is to use native R code to read the files directly,
with no reliance on the FIT SDK or other FIT parsing tools. As such it
should be platform independent, and not require any additional software
outside of a working version of R.

**FITfileR** should be considered a work in progress, and many features
available in the complete SDK are not currently implemented.

Feel free to open an
[issue](https://github.com/grimbough/FITfileR/issues) if something
doesn’t work or you notice a feature you’d like adding.

Installing from GitHub
======================

Currently **FITfileR** is only available on Github, and can be installed
using the **[remotes](https://cran.r-project.org/package=remotes)
package.**

    if(!requireNamespace("remotes")) {
        install.packages("remotes")
    }
    remotes::install_github("grimbough/FITfileR")

Usage
=====

You can find a vignette with examples of how to use the code at
<a href="https://msmith.de/FITfileR/articles/FITfileR.html" class="uri">https://msmith.de/FITfileR/articles/FITfileR.html</a>

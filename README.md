# PBOX <img src="pboxIcon.png" width="200" align="right"/>

### Version 0.0.1

<!-- badges: start -->

[![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://github.com/bcgov/repomountie/blob/8b2ebdc9756819625a56f7a426c29f99b777ab1d/doc/state-badges.md) [![License](https://img.shields.io/badge/License-GPL%303.0-blue.svg)](https://opensource.org/licenses/GPL-3.0) <!--[![R build status](https://github.com/bcgov/bcmaps/workflows/R-CMD-check/badge.svg)](https://github.com/bcgov/bcmaps/actions)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/bcmaps)](https://cran.r-project.org/package=bcmaps) [![CRAN Downloads](https://cranlogs.r-pkg.org/badges/bcmaps?color=brightgreen)](https://CRAN.R-project.org/package=bcmaps) 

<!-- badges: end -->

## Overview

The [pbox](https://github.com/athammad/pbox) R package is designed for risk assessment and management. It is an advanced statistical library that excels in exploring probability distributions within a given dataset. The tool offers a method to encapsulate and query the probability space effortlessly. Its distinctive feature lies in the ease with which users can navigate and analyze marginal, joint, and conditional probabilities while taking into account the underlying correlation structure inherent in the data. This unique capability empowers users to delve into intricate relationships and dependencies within datasets, providing a solid foundation for making well-informed decisions in the context of risk management scenarios. With pbox is straightforward to answer questions like:

-   What is the probability of experiencing extreme heat waves in Indonesia with temperatures above 32 degrees?

-   What is the probability of simultaneous extreme heat waves in Vietnam with temperatures above than 31 degrees and the average regional temperature being above than 26 degrees?

-   Given that the average regional temperature is 26 degrees, what is the probability of experiencing extreme heat waves in both Vietnam and Indonesia with temperatures above 33 degrees?

## Features

**Generate a `pbox` object from data**

```{r, echo=TRUE, eval=FALSE}
data("SEAex")
pbx<-set_pbox(SEAex)
pbx
```

**access the data and the copula object**

```{r, echo=TRUE, eval=FALSE}
pbx@data
pbc@copula
```

**access the results of the automated selection for both the marginal distribution and the copula**

```{r, echo=TRUE, eval=FALSE}
pbx@fit
```

**Explore the probabilistic space**

```{r, echo=TRUE, eval=FALSE}
#Get marginal distribution
pbx["Malaysia:33",]
#Get Joint distribution
pbx["Malaysia:33 & Vietnam:34",]
#Get Joint distribution
pbx["Vietnam:31", "avgRegion:26"]
#Conditional distribtuion Pr(X <= x, Y <= y) / Pr(Y <= y)
pbx["Malaysia:33 & Vietnam:31", "avgRegion:26"]
#Conditional distribtuion Pr(X <= x, Y <= y) / Pr(Y = y)
pbx["Malaysia:33 & Vietnam:31", "avgRegion:26",fixed=TRUE]
# Joint distribution with values set on their respective mean value
pbx["mean:c(Vietnam,Thailand)",lower.tail=T]
# Joint distribution with values set on their respective median value
pbx["median:c(Vietnam, Thailand)",lower.tail=T]
# Joint distribution with xxxx
pbx["Malaysia:33 & mean:c(Vietnam, Thailand)",lower.tail=T]
# Condtional distribtuion distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
pbx["Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)"]

```

## Installation

`install.packages("pbox")`

To install the development version of the `pbox` package, you need to install the `remotes` package then the `pbox` package.

```         
install.packages("remotes")
remotes::install_github("athammad/pbox")
```

```{=html}
<!-- 
## Vignettes

After installing the package you can view vignettes by typing `browseVignettes("syntCF")` in your R session. 
<!--  Or you can check this [document](https://github.com/athammad/syntCF/doc/syntCF_vignette.html) where you will find an example with simulated data and method references.
 -->
```
## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/athammad/pbox/issues/).

```{=html}
<!-- 
## Cite
 -->
```

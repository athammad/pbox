# PBOX <img src="./other/pboxIcon.png" align="right" height="138"/>

### Version 0.1.1 (BETA)

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

**Access the data and the copula object**

```{r, echo=TRUE, eval=FALSE}
pbx@data
pbc@copula
```

**Access the results of the automated selection for both the marginal distribution and the copula**

```{r, echo=TRUE, eval=FALSE}
pbx@fit
```

**Explore the probabilistic space**

```{r, echo=TRUE, eval=FALSE}

#Get marginal distribution
qpbox(pbx,marginal = "Malaysia:33")

#Get Joint distribution
qpbox(pbx,marginal = "Malaysia:33 & Vietnam:34")

# Conditional distribution distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,marginal = "Malaysia:33 & median:c(Vietnam,Thailand)", conditional="mean:c(avgRegion)", fixed=TRUE)

# Estimate confidence intervals
qpbox(pbx,marginal = "Vietnam:31 & avgRegion:26", conditional="Malaysia:32",CI=T)


```

## Installation
The library is currently being developed at a fast pace with the aim of being available on CRAN soon. The development version can be installed as follows:
```         
install.packages("remotes")
remotes::install_github("athammad/pbox")
```

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/athammad/pbox/issues/).

## TO DO


-   Add tests for all functions
-   Finalise Vignette
-   Improve documentation



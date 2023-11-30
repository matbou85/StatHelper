

# StatHelper: Help users to choose which statistical test to perform

> [!WARNING] 
> THIS FUNCTION IS UNDER ACTIVE DEVELOPMENT. TRY AT YOUR OWN RISK IF YOU REALLY WANT TO, OTHERWISE WAIT UNTIL A STABLE VERSION IS PUBLISHED.

`StatHelper` is a function to help the users in making a decision on an appropriate statistical test


## Installation

<!-- remove this when released to CRAN

Please install the stable release from CRAN:

``` r
install.packages("StatHelper")
```

-->


Alternatively, you can install the latest development version from github:

``` r
remotes::install_github("matbou85/StatHelper")
```


## Basic usage

``` r

StatHelper(x = myData, group = myData$group, var1 = myData$weight, paired = T) ## paired-sample t.test

```






## peeracs

__peeracs__ is an R package that wraps around the tidycensus `get_acs()` function to cleanly obtain time series data for a region of counties, peer MSAs, and the nation. Currently, it provides the function `assemble_peer_acs()` and a function manual that explains how to employ it. 

To install from github:
```r
install.packages("devtools")
library(devtools)
install_github("tallishmatt/peeracs")
library(peeracs)
```


`get_acs()` quietly collects 1-year ACS data for a set of counties, a set of peer MSAs, and the country. It collects data for a range of years, and if asked can iterate through racially-specific detailed census tables. It tracks suppressed data, and attempts to replace suppressed 1-year data with data from the 5-year ACS. Finally, it sums or calculates a weighted average for the set of counties. It defaults to CMAP's 7 county region, but this can be overridden for use by other regions.

## assemble_peer_acs arguments
```r
assemble_peer_acs(table = NULL, variables = NULL, years = 2018,
  peers = NULL, racial = FALSE, race = NULL, try_suppressed = TRUE,
  avg_weight = NULL, state_fips = "17",
  counties = c("031", "043", "089", "093", "097", "111", "197")))
}
```

## I'm new to this!
Please feel free to suggest any edits to code for style or efficiency best practices. 
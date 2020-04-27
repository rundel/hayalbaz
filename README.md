
# Hayalbaz

Puppeteer in a different language - this R package provides a puppeteer
inspired interface to the Chrome Devtools Protocol using chromote.

## Installation

``` r
remotes::install_github("rstudio/chromote")
remotes::install_github("rundel/hayalbaz")
```

## Usage

``` r
library(hayalbaz)

test = puppet$new("https://example.com")

test$get_element("p")
test$click("a")
test$view()
```

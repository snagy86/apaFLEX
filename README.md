
<!-- README.md is generated from README.Rmd. Please edit that file -->

# apaFLEX

<!-- badges: start -->

<!-- badges: end -->

`apaFLEX` calculates and formats customisable APA 7 styled tables using
the package. Producing APA 7 formatted tables in R usually means writing
a lot of repetitive code, so apaFLEX was designed to save time and cut
that down. Notably, `apaFLEX` also includes an argument that lets users
download the formatted table as a Word document. This avoids the tedium
of manually creating and re-editing tables in Word, as changes can be
made in R and copied over to the working document.

It’s currently very early in development, and can create descriptive
statistics tables and demographic count tables. However, the code is
likely still buggy, so use with caution.

## Installation

You can install the development version of apaFLEX from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("snagy86/apaFLEX")
```

## Example

Creating descriptive statistics table from mtcars dataset.

``` r
library(apaFLEX)
 
cars_table <- apa_descriptives(
  data = mtcars,
  vars = c("mpg" = "Miles Per Gallon", "hp" = "Horsepower"),
  group = "am",
  group_labels = c("0" = "Automatic", "1" = "Manual"),
  title = list(number = "1", text = "Descriptive Statistics by Transmission Type"),
  note = "an APA formatted note",
  footnotes = list("mpg" = "a footnote regarding mgp appears hear"))

print(cars_table)
#> a flextable object.
#> col_keys: `Variable`, `M_Automatic`, `SD_Automatic`, `spacer_1`, `M_Manual`, `SD_Manual` 
#> header has 4 row(s) 
#> body has 2 row(s) 
#> original dataset sample: 
#> 'data.frame':    2 obs. of  6 variables:
#>  $ Variable    : chr  "Miles Per Gallon" "Horsepower"
#>  $ M_Automatic : chr  "17.15" "160.26"
#>  $ SD_Automatic: chr  "3.83" "53.91"
#>  $ spacer_1    : chr  "" ""
#>  $ M_Manual    : chr  "24.39" "126.85"
#>  $ SD_Manual   : chr  "6.17" "84.06"
```

Creating demographic count table for for mtcars dataset.

``` r
set.seed(11)

df <- data.frame(
  Gender = sample(c("Woman", "Man", "Non-binary"), 100, replace = TRUE),
  Ethnicity = sample(c("White", "Black", "Asian", "Hispanic", "mixed"), 100, replace = TRUE),
  Condition = sample(c("A", "B"), 100, replace = TRUE)
)

apa_demographics(
  data = df,
  demo_vars = c("Gender", "Ethnicity"),
  group = "Condition",
  group_labels = c("A" = "Control", "B" = "Treatment"),
  total = TRUE,
  title = list(number = "1", text = "Demographics Table by Condition"),
  note = "Note in table 2",
  footnotes = list("Gender" = "here is letter a", "White" = "here is letter b")
)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" />

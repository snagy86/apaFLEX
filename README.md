
<!-- README.md is generated from README.Rmd. Please edit that file -->

# apaFLEX

<!-- badges: start -->

<!-- badges: end -->

`apaFLEX` calculates and formats customisable APA 7 styled tables using
the `flextable` package. Producing APA 7 formatted tables in R usually
means writing a lot of repetitive code, so apaFLEX was designed to save
time and cut that down. Notably, `apaFLEX` also includes an argument
that lets users download the formatted table as a Word document. This
avoids the tedium of manually creating and re-editing tables in Word, as
changes can be made in R and copied over to the working document.

The package is currently very early in development after being tabled
while I focused on my main project, the package `diy.sem.plot`
(<https://github.com/snagy86/diy.sem.plot>), which is waiting for manual
CRAN inspection.

`apaFLEX` can currently create descriptive statistics tables and
demographic count tables. However, the code is likely still buggy, so
please use with caution.

## Installation

Run the following code to download the development version of the
package.

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
  footnotes = list("mpg" = "a footnote regarding mpg appears hear"))

cars_table
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

Creating demographic count table for for mtcars dataset.

``` r
set.seed(11)

df <- data.frame(
  Gender = sample(c("Woman", "Man", "Non-binary"), 100, replace = TRUE),
  Ethnicity = sample(c("White", "Black", "Asian", "Hispanic", "mixed"), 100, replace = TRUE),
  Condition = sample(c("A", "B"), 100, replace = TRUE)
)

demographics_table <- apa_demographics(
  data = df,
  demo_vars = c("Gender", "Ethnicity"),
  group = "Condition",
  group_labels = c("A" = "Control", "B" = "Treatment"),
  total = TRUE,
  title = list(number = "2", text = "Demographics Table by Condition"),
  note = "Note in table 2",
  footnotes = list("Gender" = "here is letter a", "Ethnicity" = "here is letter b")
)

demographics_table
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" />

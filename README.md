<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# DataTutor: Data Science Code Comprehension Tools.

A project that uses Shiny interactive apps for facilitating Data Science code comprehension using R.

**NOTE:** This is not yet ready for public use, and is undergoing development. But, if you are ever so curious, you can install it with:

```r
devtools::install_github('nischalshrestha/DataTutor')
```

Then, you can try unraveling `dplyr` or `tidyr` (pivoting) code like so:

```r
DataTutor::unravel(
  mtcars %>%
    group_by(cyl) %>% 
    summarise(mean_mpg = mean(mpg))
)
```

This will open up a Shiny web app on the Viewer pane in RStudio by default. But, if you want to respect your currently chosen browser window, you can add a `viewer = FALSE`:

```r
  DataTutor::unravel(
    mtcars %>%
      group_by(cyl) %>% 
      summarise(mean_mpg = mean(mpg)),
    viewer = FALSE
  )
```


# seafisheries

<!-- badges: start -->
<!-- badges: end -->

The goal of seafisheries is to ...


## Getting started

Vignettes are the main entry point for this package. They are designed to be
run in order and each one produces objects used by the next.

To access the vignettes after installing the package:
```r
# List available vignettes
vignette(package = "seafisheries")

# Open a specific vignette
vignette("setup", package = "seafisheries")
```

Alternatively, open the `.qmd` files directly in RStudio from the `vignettes/`
folder and run them interactively block by block.

### Vignette order

| # | File | Description |
|---|------|-------------|
| 01 | `setup.qmd` | Load spatial masks and set up the environment. Run this first. |
| 02 | `operational_data.qmd` | Clean and format raw fisheries logbook data. |
| 03 | `clusters.qmd` | Cluster cleaned data to identify fishing strategies. |

### Installation
```r
# install.packages("devtools")
devtools::install_github("your-org/seapodym-clusterFisheries",
                          build_vignettes = TRUE)
```

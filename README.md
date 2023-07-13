
# imdccal

<!-- badges: start -->
<!-- badges: end -->

The goal of imdccal is to extract data from water chemistry lab deliverables and convert it to a machine readable format.

## Installation

You can install the development version of imdccal from [its GitHub repository](https://github.com/nationalparkservice/nccn-climate) with:

``` r
# install.packages("remotes")
remotes::install_github("nationalparkservice/imd-ccal")
```

## Example

Read data from a single file of CCAL lab data and write a machine-readable version to an Excel file or set of CSV files:

``` r
library(imdccal)
# We'll assume that you have the CCAL file and a folder called "ccal_tidy" in your working directory
machineReadableCCAL("LVLD_101118.xlsx", destination_folder = "ccal_tidy")  # Write tidied data to a new .xlsx
machineReadableCCAL("LVLD_101118.xlsx", format = "csv", destination_folder = "ccal_tidy")  # Write tidied data to a folder of CSV files
```

Read data from *multiple* files of CCAL lab data and write a machine-readable version to an Excel files or sets of CSV files:

``` r
library(imdccal)
# We'll assume that you have a folder full of CCAL files in a folder called "ccal", and a folder called "ccal_tidy" in your working directory
all_files <- list.files("ccal", pattern = "*.xlsx$", full.names = TRUE)
machineReadableCCAL(all_files, destination_folder = "ccal_tidy")  # Write one file of tidied data per input file
machineReadableCCAL(all_files, format = "csv", destination_folder = "ccal_tidy")  # Write one folder of tidied CSV data per input file
```

Read and tidy the data and work with it in R, without writing the data to any files:

``` r
library(imdccal)
all_files <- list.files("ccal", pattern = "*.xlsx$", full.names = TRUE)
tidy_ccal <- getCCALData(all_files)
lvld_18_data <- tidy_ccal$`LVLD_101118.xlsx`$data  # Get the data for a single set of lab results
lvld_18_meta <- tidy_ccal$`LVLD_101118.xlsx`$metadata # Get the metadata for the same set of results
```


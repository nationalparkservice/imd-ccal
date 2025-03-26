tryParseDate <- function(date_chr, try_fns = list(lubridate::mdy_hms, lubridate::mdy, lubridate::ymd_hms, lubridate::ymd)) {
  count_na <- sum(is.na(date_chr)) # Check number of NA's in orig data. We want to make sure our date conversion attempts don't create more NA's.

  # Try date conversions until one (hopefully) works
  for (convert_fn in try_fns) {
    success <- TRUE
    new_dates <- tryCatch(janitor::convert_to_datetime(date_chr, character_fun = convert_fn, tz = "America/Los_Angeles", string_conversion_failure = "error"),
                          error = function(e){success <<- FALSE},
                          warning = function(w){success <<- FALSE})
    if (success) {
      break
    }
  }

  # If date conversion failed or yielded additional missing data, revert to the original data
  if (!(success && sum(is.na(new_dates)) == count_na)) {
    new_dates <- date_chr
  }

  return(new_dates)
}

#' Expand Number Range in Text to All Numbers in Range
#'
#' Takes a character vector containing a range of numeric IDs (e.g. "11 - 14") and replaces the range with the numbers contained in the range ("11 12 13 14").
#' @details
#' Does NOT work with negative numbers.
#'
#'
#' @param numbers Character vector containing a number range indicated by a dash (e.g. "123 - 456"). Numbers outside of range must be separated by whitespace (e.g. "1 3 5-9"). Number range can be ascending or descending but cannot contain negative numbers. "-123 - 456" is NOT valid input. No non-numeric characters are permitted, except for whitespace and dashes.
#' @param separator Character string to use as a separator between numbers in output.
#'
#' @return The same character vector, but with number ranges removed and replaced with the numbers in the range explicitly listed.
#'
range_to_vector <- Vectorize(function(numbers, separator = " ") {
  if (is.na(numbers) || length(numbers) == 0) {
    return(NA)
  }
  numbers <- stringr::str_replace_all(numbers, "\\s+", " ")  # replace all whitespace with single space
  ranges <- unlist(stringr::str_extract_all(numbers, "\\d+\\s*(\u2013|\u2D)\\s*\\d+"))  #\u2D and \u2013 are unicode dashes
  sapply(ranges, function(range) {
    start <- stringr::str_extract(range, "^\\d+")
    end <- stringr::str_extract(range, "\\d+$")
    seq <- paste(seq(start, end), collapse = separator)
    numbers <<- stringr::str_replace(numbers, range, seq)
  })

  # Check for non-numeric characters in final result and throw error if found
  if (grepl(pattern = "[^\\d|\\s]", numbers, perl = TRUE)) {
    cli::cli_abort("Numeric IDs are improperly formatted. Special characters (except for a dash indicating a numeric range) are not supported, nor are negative IDs.")
  }

  return(numbers)
}, USE.NAMES = FALSE)

#' List every parameter that occurs in the data
#'
#' @param ccal_data The list object returned by `read_ccal`
#'
#' @return A character vector of parameter codes
#' @export
#'
#' @examples
#' my_folder <- "ccal_results"
#' file_list <- list.files(my_folder, pattern = "*.xlsx$", full.names = TRUE)
#' all_ccal_data <- read_ccal(file_list)
#' parameters <- get_data_params(all_ccal_data)
get_data_params <- function(ccal_data) {
  unique(
    unlist(
      lapply(ccal_data, function(l) {
        params <- unique(l$data$parameter)
        return(params)
      }
      )
    )
  )
}

#' List every parameter name that occurs in the questionable results
#'
#' @param ccal_data The list object returned by `read_ccal`
#'
#' @return A character vector of parameter names
#' @export
#'
#' @examples
#' my_folder <- "ccal_results"
#' file_list <- list.files(my_folder, pattern = "*.xlsx$", full.names = TRUE)
#' all_ccal_data <- read_ccal(file_list)
#' param_names <- get_questionable_params(all_ccal_data)
get_questionable_params <- function(ccal_data) {
  unique(
    unlist(
      lapply(ccal_data, function(l) {
        qa_params <- unique(l$questionable$param_description)
        return(qa_params)
      }
      )
    )
  )
}

#' List the file name(s) or file paths of imdccal's example data
#'
#' @param file_names Optional. File name of requested example data. For a list of available example data files, omit this argument.
#'
#' @returns If file_names is supplied, `use_example_data()` returns the full file path to the requested package data.
#' Otherwise, it returns the file names of example data.
#' @export
#'
#' @examples
#' # List all example data file names
#' use_example_data()
#'
#' # Find file path to one example data file
#' use_example_data(file_names = "SPAC_080199.xlsx")
#'
#' # Find file path to all example data files
#' use_example_data(file_names = use_example_data())
use_example_data <- function(file_names = NULL) {
  if (is.null(file_names)) {
    dir(system.file("extdata", package = "imdccal"))
  } else {
    system.file("extdata", file_names, package = "imdccal", mustWork = TRUE)
  }
}

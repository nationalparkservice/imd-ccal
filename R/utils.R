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
#' Takes a character vector containing a number range (e.g. "11 - 14") and replaces the range with the numbers contained in the range ("11 12 13 14").
#'
#' @param numbers Character vector containing a number range indicated by a dash (e.g. "123 - 456")
#' @param separator Character string to use as a separator between numbers
#'
#' @return The same character vector, but with number ranges removed and replaced with the numbers in the range explicitly listed.
#' @export
#'
#' @examples
#' rangeToVector("1 3 5-8 10")
rangeToVector <- Vectorize(function(numbers, separator = " ") {
  if (is.na(numbers) || length(numbers) == 0) {
    return(NA)
  }
  ranges <- unlist(stringr::str_extract_all(numbers, "\\d+\\s*(–|-)\\s*\\d+"))
  sapply(ranges, function(range) {
    start <- stringr::str_extract(range, "^\\d+")
    end <- stringr::str_extract(range, "\\d+$")
    seq <- paste(seq(start, end), collapse = separator)
    numbers <<- stringr::str_replace(numbers, range, seq)
  })

  return(numbers)
}, USE.NAMES = FALSE)


#' List every parameter that occurs in the data
#'
#' @param ccal_data The list object returned by `getCCALData`
#'
#' @return A character vector of parameter codes
#' @export
#'
#' @examples
#' my_folder <- "ccal_results"
#' file_list <- list.files(my_folder, pattern = "*.xlsx$", full.names = TRUE)
#' all_ccal_data <- getCCALData(file_list)
#' parameters <- listParamsInData(all_ccal_data)
listParamsInData <- function(ccal_data) {
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
#' @param ccal_data The list object returned by `getCCALData`
#'
#' @return A character vector of parameter names
#' @export
#'
#' @examples
#' my_folder <- "ccal_results"
#' file_list <- list.files(my_folder, pattern = "*.xlsx$", full.names = TRUE)
#' all_ccal_data <- getCCALData(file_list)
#' param_names <- listParamsInQuestionableResults(all_ccal_data)
listParamsInQuestionableResults <- function(ccal_data) {
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
#' @param file_names
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

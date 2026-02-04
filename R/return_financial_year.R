#' Get the start year of the given financial year
#'
#' This function returns the start year of the current financial year based on a given date.
#' The financial year is assumed to start on April 1st and end on March 31st of the following year.
#'
#' @param input_date A date in "YYYY-MM-DD" format or a Date object.
#'
#' @return An integer representing the start year of the financial year (e.g., 2025 for FY 2025/26).
#'
#' @export
#'
#' @examples
#' return_financial_year(input_date = "2025-10-29")  # Returns 2025
#' return_financial_year("2026-02-04")              # Returns 2025
return_financial_year <- function(input_date) {

  # Try converting to Date
  input_date <- as.Date(input_date)

  # Check if conversion succeeded
  if (is.na(input_date)) {
    stop("input_date must be a valid date string or Date object")
  }

  # Extract year and month
  year <- as.integer(format(input_date, "%Y"))
  month <- as.integer(format(input_date, "%m"))

  # Return financial year start
  if (month < 4) return(year - 1) else return(year)
}

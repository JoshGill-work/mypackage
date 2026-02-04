current_financial_year <- function() {
  today <- Sys.Date()
  year <- as.integer(format(today, "%Y"))
  month <- as.integer(format(today, "%m"))

  if (month < 4) {
    return(year - 1)
  } else {
    return(year)
  }
} q

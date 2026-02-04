return_financial_year <- function(input_date) {
  #Ensure the input_date is a date object
  input_date <- as.Date(input_date)

  # Takes the year and month value as the
  year <- as.integer(format(input_date, "%Y"))
  month <- as.integer(format(input_date, "%m"))

  # If month is < 4, return the previous year, else return the current year
  if (month < 4) {
    return(year - 1)
  } else {
    return(year)
  }
}

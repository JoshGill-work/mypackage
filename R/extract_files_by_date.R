#' File Identification and Extraction.
#'
#' @description
#' extract_file_by_date identifies files matching a partial file string and returns those with a date string in their name which is later than a given date. This function is designed to identify files which are regularly produced (data extracts) and to identify potential files which could contain data for processing a within a given time period.
#'
#' The function is primarily designed to filter out historic data extracts for a given report and identify potential ones which contain the relevant data. Can be utilised to ensure full Periods worth of data is obtained when loaded in and filtered.
#'
#'
#' @param partial_file_name a character string which is a partial file name
#' @param directory a character string which is the directory path for the function to identify files from
#' @param date date
#'
#' @returns a vector of file names as strings
#' @export
#'
extract_files_by_date <- function(partial_file_name,
         directory,
         date
) {
  # Check that the directory exists
  if (!dir.exists(directory)) {
    stop("The specified directory does not exist.")
  }

  # Identify the files within the directory
  files <- list.files(path = directory,
                      full.names = FALSE)

  # Filter the files within the directory to return only those starting with the partial_file_name
  matched_files <- files[grepl(partial_file_name, files)] # grepl identifies the elements within files that contain the string partial_file_name and then returns those elements in a list

  # Extracts the date strings from the matched file names
  extracted_file_dates <- as.Date(stringr::str_extract(string = matched_files,
                       pattern = "\\d{4}-\\d{2}-\\d{2}"))

  # Checks to see which of the extracted_file_dates are after a given date
  files_after_date <- extracted_file_dates > date

  # Return the file names of the files that were made after date
  filtered_file_names <- matched_files[files_after_date]

return(filtered_file_names)
}

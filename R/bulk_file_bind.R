bulk_file_bind <- function(
    file_path,
    partial_file_name,
    ignore.case = TRUE,
    recursive = FALSE, # By default, this function won't look within sub directories unless specified TRUE
    full.names = FALSE, # By default, this function will only return the file name, not the full directory path
    fixed = TRUE,
    output_file_name
) {

  # Input Checks
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("`file_path` must be a single file path (character).")
  }
  if (!dir.exists(file_path)) {
    stop("file_path does not exist: ", file_path)
  }
  # Identify the list of files
  list_of_files <- list.files(path = file_path,
                              recursive = recursive
  )

  matching_files <- grepl(pattern = partial_file_name,
                          x = list_of_files,
                          ignore.case = ignore.case,
                          fixed = fixed)

  matching_file_names <- list_of_files[matching_files]

  # Test - Check number of found files or return message

  if (length(matching_file_names) == 0L) {
    message("No files were matched, please check your input parameters and try again")
  } else {
    message(paste0(length(matching_file_names), " match(es) found"))
  }

  # Convert the matching_file_names full name path if not

  matching_file_paths <- if (full.names) matching_file_names else file.path(file_path,matching_file_names)


  # Combine the data extracts together and add a column to identify where the data came from
  combined_with_source <- map_dfr(matching_file_paths, ~ read_excel(.x) %>% mutate(source = basename(.x))) %>%
    relocate(source,.before = 1)

  # Export the file as an Excel

  write.xlsx(combined_with_source, file = output_file_name)



  return("File has been successfully created")
}

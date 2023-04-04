#' Function to find file names that match the specified pattern
#'
#' @param file_path_death_data: the file path to look for the file names
#' @param regex_pattern: string indicating the regex pattern
#'
#' @return: a list of file names
find_file_names <- function(file_path_death_data, regex_pattern) {
  # Use regex to identify all the attributes files which end in the regex pattern then .xlsx
  files <- list.files(path = file_path_death_data, pattern = regex_pattern)

  return(files)
}

#' Function to extract the attribute groups from the given files
#'
#' @param maternal_death_files: vector of characters/strings containing the files to process
#' @param dates: the dates in the file names to exclude 
#'
#' @return: a list of attribute groups
extract_attribute_groups <- function(maternal_death_files, dates) {
  attribute_groups <- maternal_death_files %>%
    gsub(paste0("Maternal Deaths ", dates, " "), "", .) %>%
    gsub("\\.xlsx", "", .)

  return(attribute_groups)
}

#' Function to read in and clean up maternal deaths
#'
#' @param maternal_death_files: the files to append onto the end of file_path_death_data
#' @param file_path_death_data: the file path prefix to read from
#' @param attribute: the attribute of mother determining how to group maternal deaths
#'
#' @return: a tibble of maternal deaths for all attribute groups over all years
maternal_deaths <- function(maternal_death_files, file_path_death_data, attribute) {

  # Map through all the files reading in the excel files and cleaning them up
  # Then join them all together as a long dataframe
  maternal_deaths <- maternal_death_files %>%
    map(~ file.path(file_path_death_data, .x)) %>%
    map_dfr(~ read_excel(.x, skip = 11) %>% # Read in file
      rename(
        year = `...1`, # Fix a few column names
        total = last_col()
      ) %>%
      filter(total > 0) %>% # This is the exact filter to keep the right rows
      slice(-1) %>% # Remove the total row
      select(-total) %>% # Remove the total column
      mutate_all(as.numeric) %>% # Make everything numeric
      pivot_longer(-year, names_to = "county", values_to = "maternal_deaths"), # pivot
    .id = attribute # Append attribute of mother as a column
    ) %>%
    select(year, county, !!attribute, maternal_deaths) %>% # Re-order columns
    mutate(!!attribute := pluck(., attribute) %>% as.factor() %>% fct_inorder())
  # Make attribute of mother a properly ordered factor so the visualization later is nice

  return(maternal_deaths)
}

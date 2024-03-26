# clean_licor_files(directory_path, file_type, write_to_csv, write_to):
#
# Description: helper function that reads all tab-delimited UTF-8 or .xlsx
# files in a folder directory and converts all files into a tidy, cleaned file.
# The function gives the option to write all cleaned files into a separate
# folder, or return a list of data frames with each data frame containing data
# for a single LI-6800 file.
#
# Function inputs:
#
# - directory_path    = folder containing a selection of uncleaned source 
#                       LI-6800 files
# - write_directory   = new folder where cleaned LI-6800 files are stored. Files are
#                       automatically saved as a .csv file with no option for 
#                       writing files as a different extension. This function can
#                       easily be manually altered, however, to write files to a 
#                       different extension
# - return_list       = specifies whether function returns cleaned data files 
#                       as a list of data frames (return_list = TRUE), or writes
#                       
#
# Returns:
# - writes a .csv file for each source LI-6800 file into the folder designated as
#   the write_directory
# - source LI-6800 files are compiled into a single list of data frames
clean_licor_files <- function(directory_path,
                              write_directory,
                              return_list = FALSE) {
  
  ## Assign directory path
  file_list <- list.files(directory_path, full.names = TRUE)

  ## Apply `clean_licor_file` across all files in directory_path 
  cleaned_files_list <-  lapply(file_list, 
                                function(x) 
                                  clean_licor_file(path = x, 
                                                   write_to_csv = FALSE))
  
  ## Change list elements to the basename of the file_list
  names(cleaned_files_list) <- basename(file_list)
  
  if(return_list) {
    
    return(cleaned_files_list)
  } else if(!return_list) {

    for(i in seq_along(cleaned_files_list)) {
      write.csv(cleaned_files_list[[i]],
                file = paste0(file.path(write_directory, 
                                        names(cleaned_files_list[i])),
                              "_cleaned.csv"), row.names = FALSE)
    }
    
  }
}  



clean_licor_files(directory_path = "../2022_NxCO2xI/licor_raw/week7",
                          return_list = FALSE,
                          write_directory = "/Users/eaperkowski/Desktop/test_licor_script")





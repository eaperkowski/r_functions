# clean_licor_files(directory_path, file_type, write_to_csv, write_to):
#
# Description: helper function that reads a tab-delimited UTF-8 or .xlsx
# Microsoft Excel file and converts it into a tidy, cleaned file. 
# 
# The function returns the tidy file using one of two output options: 
#   - (1) a .csv file that gets written to the path of the user's choosing
#   - (2) as a data frame
#
# Function inputs:
#
# - directory_path    = folder containing suite of uncleaned source LI-6800 files
# - file_type         = string that designates whether source LI-6800 files are
#                       a tab-delimited UTF-8 file or a Microsoft Excel (.xlsx) file
# - write_to_csv      = Boolean operator. If write_to_csv = TRUE, the function will
#                       writes all newly cleaned source lI-6800 files as a .csv into 
#                       the folder designated in the write_directory argument. If
#                       write_to_csv = FALSE, the function will compile all cleaned
#                       source LI-6800 files into a list of data.frames
# - write_directory   = new folder where cleaned LI-6800 files are stored. Files are
#                       automatically saved as a .csv file with no option for 
#                       writing files as a different extension. This function can
#                       easily be manually altered, however, to write files to a 
#                       different extension
#
# Returns:
# - writes a .csv file for each source LI-6800 file into the folder designated as
#   the write_directory
# - source LI-6800 files are compiled into a single list of data frames
clean_licor_files <- function(directory_path = "",
                              write_directory = "",
                              skip_rows = 63,
                              return_list = FALSE) {
  
  ## Assign directory path
  file_list <- list.files(directory_path, full.names = TRUE)

  
  for (file in file_list) {
    
    licor_cleaned <- clean_licor_file(file, 
                                      skip_rows = skip_rows, 
                                      write_to_csv = FALSE)
    
    if(return_list) {
      
      licor_cleaned_list[[basename(file)]] <- list(data = licor_cleaned, 
                                                   file_name = basename(file))
      } else {
      
        file_to_write <- file.path(write_directory, basename(file))
      
        write.csv(licor_cleaned, file = paste0(file_to_write, ".csv"))
    }
    
    if(return_list) {
      return(licor_cleaned_list)
      }

  }
}

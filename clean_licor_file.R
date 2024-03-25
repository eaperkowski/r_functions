###############################################################################
# clean_licor_file(path, file_type, write_to_csv, write_to):
###############################################################################
#
# Description: helper function that reads an uncleaned LI-6800 source file 
# (compatitble with both a UTF-8 or .xlsx source file) and converts it into
# a clean and tidy file. The helper function will be implemented in a second
# function (`clean_licor_files`) to automate cleaning all LI-6800 source files
# in a working directory.
#
# Arguments:
# - path                = path of uncleaned .txt LI-6800 file
# - write_to_csv        = Boolean operator that dictates whether cleaned script 
#                         gets written as a .csv folder (write_to_csv = TRUE) or 
#                         returned as an object (write_to_csv = FALSE)
# - write_to            = path to write cleaned .txt LI-6800 file. Only used if 
#                         write_to_csv = TRUE
# - common_params_only  = Boolean operator that dictates whether the function
#                         filters cleaned source file to only include commonly
#                         used plant ecophys traits. Note: does not include
#                         fluorescence measurements but function can be easily
#                         adapted for this
#
# Returns:
# - a .csv file written to a path of user's choosing (if write_to_csv = TRUE)
# - a data frame (if write_to_csv = FALSE)
clean_licor_file <- function(path = "",
                             common_params_only = FALSE,
                             write_to_csv = TRUE,
                             write_to = "") {
  
  # Determine whether read-in file is an excel file or UTF-8.
  # Boolean: if is_excel == TRUE, then read file using readxl::read_excel.
  # if is_excel == FALSE, then read file using utils::read.table
  is_excel <- grepl(".xlsx$", path)
  
  if(is_excel) {
    # Read file using readxl::read_excel
    data <- suppressMessages(readxl::read_excel(path = path,
                                                sheet = 1,
                                                col_names = TRUE))

  } else if(!is_excel){ # Read file using utils::read.table
    
    # Determine maximum number of columns (needed to designate col.names
    # in utils::read.table)
    col_number <- max(count.fields(file = path, sep = "\t", quote = ""))

    # Read file using utils::read.table
    data <- utils::read.delim(file = path, sep = "\t", row.names = NULL, 
                              col.names = 1:col_number, header = FALSE)
  }
  
  # Remove all rows before "[Data]"
  data_noheader <- data[cumsum(data$X1 == "[Data]") >= 1, ]
  
  # Rename columns and remove final 
  names(data_noheader) <- data_noheader[3, ]
  data_clean <- data_noheader[-c(1:4), ]
  
  # Subset dataset to only include columns relevant for A/Ci curves
  if(common_params_only) {
    data_clean <- dplyr::select(data_clean, obs, date, machine, id,
                                A, Ca, Ci, gsw, Tleaf, Qin)
  } else if(!common_params_only) {
    data_clean
  }
  

  # Conditional return (if write_to_csv = TRUE, then .csv file is written.
  # if write_to_csv = FALSE, then cleaned file is returned as an object)
  if(write_to_csv == TRUE) { 
    
    # Create file_name path
    file_name <- paste0(basename(path), ".csv")
    
    write.csv(data_clean,
              file = file.path(write_to, file_name),
              row.names = FALSE) }
  else{
    return(data_clean)
    }
}

#############
## TESTING ##
#############
#clean_licor_file(path = "../2022_NxCO2xI/licor_raw/week7/aco2_co2Resp_W7D2_alb",
#                 common_params_only = TRUE,
#                 write_to_csv = FALSE,
#                 write_to = "")

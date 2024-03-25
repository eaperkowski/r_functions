# clean_licor_file(path, file_type, write_to_csv, write_to):
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
# - path              = path of uncleaned .txt LI-6800 file
# - file_type         = string that designates whether the source LI-6800 file
#                       is a tab-delimited UTF-8 file or a Microsoft Excel
#                       (.xlsx) file
# - skip_rows         = number of rows to skip and get rid of header
#                       Default value: 63
# - write_to_csv      = Boolean operator that dictates whether cleaned script 
#                       gets written as a .csv folder (write_to_csv = TRUE) or 
#                       returned as an object (write_to_csv = FALSE)
# - write_to          = path to write cleaned .txt LI-6800 file. Only used if 
#                       write_to_csv = TRUE
#
# Returns:
#
# - a .csv file using a path of the users choosing (only if write_to_csv = TRUE)
# - a data frame with the tidy data file (can be assigned as an object)
clean_licor_file <- function(path = "",
                             skip_rows = 63,
                             write_to_csv = TRUE,
                             write_to = "") {
  
  is_excel <- grepl(".xlsx$", path)
  
  if(is_excel) {
    data <- suppressMessages(readxl::read_excel(path = path,
                                                sheet = 1,
                                                col_names = TRUE))
  } else if(!is_excel){
    data <- utils::read.table(path, skip = skip_rows, sep = "\t",
                              header = TRUE, quote = "", 
                              stringsAsFactors = FALSE, 
                              na.strings = "NA")[-1, ]
    
    data <- tibble::as_tibble(data)
  }

  ## Conditional return (if write_to_csv = TRUE, then .csv file is written.
  ## if write_to_csv = FALSE, then cleaned file is returned as an object)
  if(write_to_csv == TRUE) { 
    file_name <- paste0(basename(path), ".csv")
    
    write.csv(data,
              file = file.path(write_to, file_name),
              row.names = FALSE) }
  else{
    return(data)
    }
}


## TESTING

## Does script output a data frame using a utf-8 and excel file and write_to_csv is 
## set to FALSE?
clean_licor_file(path = "../2022_NxCO2xI/licor_raw/week7/aco2_co2Resp_W7D2_alb",
                 write_to_csv = TRUE,
                 write_to = "../../Desktop/test_fxn/")
# YES!





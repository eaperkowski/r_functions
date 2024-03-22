# clean_licor_file(path, write_to_csv, write_to):
#
# Description: helper function that reads a UTF-8 LI-6800 file and converts it
# into a tidy, cleaned file. The function returns the tidy file using one of 
# two output options: (1) as a .csv file that gets written using a path of the
# user's choosing, (2) as a data frame that can be assigned as an object.
#
# Function inputs:
#
# - path              = path of uncleaned .txt LI-6800 file
# - write_to_csv      = Boolean operator that dictates whether cleaned script 
#                       gets written to a .csv folder (write_to_csv = TRUE) or 
#                       returned as an object (write_to_csv = FALSE)
# - write_to          = path to write cleaned .txt LI-6800 file. Only used if 
#                       write_to_csv = TRUE
#
# Returns:
#
# - a .csv file using a path of the users choosing (only if write_to_csv = TRUE)
# - a data frame with the tidy data file (can be assigned as an object)
clean_licor_file_txt <- function(path = "", write_to_csv = TRUE,
                                 write_to = "") {
  
  ## Assign basename
  file_name <- basename(path)
  
  ## Determine number of cols (needed for next step to assign initial col.names)
  maxCols <- max(utils::count.fields(file = path, sep = "\t", 
                                     quote = ""))
  
  ## Read data from file path and remove 
  data <- utils::read.table(path, sep = "\t", quote = "", dec = ".", 
                            stringsAsFactors = FALSE, blank.lines.skip = FALSE, 
                            skipNul = FALSE, comment.char = "", 
                            row.names = NULL, col.names = 1:maxCols) |>
    tibble::as_tibble(data) |>
    dplyr::slice(-(1:63))

  ## Actually assign column names and remove additional two columns
  colnames(data) <- as.character(unlist(data[1, ]))
  data <- data[-c(1:2), ]

  ## Conditional return (if write_to_csv = TRUE, then .csv file is written.
  ## if write_to_csv = FALSE, then cleaned file is returned as an object)
  if(write_to_csv) { 
    write.csv(data,
              file = paste(stringr::str_c(write_to, "/", file_name, ".csv"))) }
  else{return(data)}
}


mySafeWriteCSV <- function ( data = NULL, namebase = NULL, writeRowNames = FALSE ) {
  if ( is.null( namebase ) ) {  # check if a file name was actually given
    warning("Error!  No file name given!\n")
  } else if ( is.null(data) ) {  # check if data were actually given
    warning("Error!  No data given!\n")
  } else {
    # strip off file extension if present:
    namebase <- strsplit(namebase, "\\.")[[1]][1] 
    filename <- getUnusedFilename(namebase, ".csv")
    cat(paste("Writing file to '", filename, "'\n", sep = ""))
    # finally, actually write the data to a file:
    write.csv(data, filename, row.names = writeRowNames)
  }
}

getUnusedFilename <- function(namebase, fileext = ".csv") {
  # create first name to try with extension:
  filename <- paste(namebase, fileext, sep = "") 
  counter <- 1 # variable for making new names in a logical sequence
  # use a while loop to find an unused name:
  while ( file.exists(filename) ) {
    filename <- paste(namebase, counter, fileext, sep = "")
    counter <- counter + 1
  }
  return(filename)
}

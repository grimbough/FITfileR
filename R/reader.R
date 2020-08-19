#' Read a FIT file
#' 
#' Reads a given FIT file and returns a list of data.frames, one for each
#' message type stored in the input file. 
#' 
#' @param fileName A character specifying the FIT file to be read.
#' @param dropUnknown Many FIT files contain data that is not defined in the FIT
#' file specification.  This may be used by the device manufacturer for 
#' debugging purposes, but is typically not useful to an end user.  The default
#' value of this argument will exclude these fields from the returned data 
#' structure.  Setting a value of \code{FALSE} will retain them.
#' @param mergeMessages FIT files may contain similar 'messages' with varying 
#' numbers of fields e.g. if a new sensor is added during an activity the 
#' 'records' messages recorded after this will contain an extra data column.
#' The default value of this argument will merge all messages of the same type,
#' and insert \code{NA} to pad missing fields.  Setting this to \code{FALSE}
#' will return a separate \code{data.frame} for each distinct message type.
#' @return A \code{list} of \code{tibbles}.  Each \code{tibble} holds a 
#' message type defined in the input file.  The list entries are named 
#' according to the message type.
#' 
#' @examples
#' garmin_file <- system.file("extdata/Garmin.fit", package = "fitFileR")
#' garmin <- readFitFile(garmin_file)
#' 
#' ## examine the 'file ID' messages
#' garmin$file_id
#'
#' @importFrom utils data
#' @export
readFitFile <- function(fileName, dropUnknown = TRUE, mergeMessages = TRUE) {
  
  tmp <- .readFile(fileName)
  return(tmp)
}

.readFile <- function(fileName) {
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  file_header <- fitFileR:::.readFileHeader(con)
  
  messages <- list()
  msgDefs <- list()
  count <- 1
  msg_count <- 1
  
  while(seek(con, where = NA) < (file_header$data_size + file_header$size)) {
    
    record_header <- fitFileR:::.readRecordHeader(con)
    
    if(record_header@is_definition) {
      msgDefs[[ count ]] <- fitFileR:::.readMessage_definition(con = con, message_header = record_header)
      count <- count + 1
    } else {
      definition <- fitFileR:::.matchDefinition(msgDefs, local_message_number = record_header@local_message_number)
      messages[[ msg_count ]] <- fitFileR:::.readMessage_data(con = con, 
                                                              header = record_header, 
                                                              definition = definition)
      
      if(is( messages[[ msg_count ]], "FitDataMessageWithDevData")) {
        messages[[ msg_count ]]@dev_field_details <- fitFileR:::.matchDevDefinition(messages, dev_data_idx = 0)
      }
      
      msg_count <- msg_count + 1
    }
    
  }
  
  fit <- new("RawFitFile", header = file_header, messages = messages)
  return(fit)
}

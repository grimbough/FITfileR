#' Read a FIT file
#' 
#' Reads a specified FIT file and returns an object of class \code{FitFile}
#' 
#' @param fileName A character specifying the FIT file to be read.
#' 
#' @return An object of class \code{[FitFile-class]}
#' 
#' @examples
#' garmin_file <- system.file("extdata", "Activities", "garmin-edge530-ride.fit",
#'                            package = "FITfileR")
#' garmin <- readFitFile(garmin_file)
#' 
#' @export
readFitFile <- function(fileName) {
  
  tmp <- .readFile(fileName, preallocate = TRUE)
  return(tmp)
}

#' @importFrom methods is new
.readFile <- function(fileName, preallocate = TRUE) {
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  file_header <- .readFileHeader(con)
  
  ## pre-allocating space for the list of messages saves a bit of time
  ## if there are more than 25,000 messages it will just grow anyway
  if(preallocate) {
    messages <- vector(mode = "list", length = 25000)
  } else {
    messages <- list()
  }

  msgDefs <- list()
  devMessages <- list()
  count <- 1
  msg_count <- 1
  prev_header <- NULL
  
  while(seek(con, where = NA) < (file_header$data_size + file_header$size)) {
    
    if(nzchar(Sys.getenv("DEBUG_FITFILER"))) {
      message(seek(con, where = NA), ":", file_header$data_size + file_header$size)
    }
    
    record_header <- .readRecordHeader(con, prev_header)
    
    if(isDefinition(record_header)) {
      def <- .readMessage_definition(con = con, message_header = record_header)
      msgDefs[[ as.character(localMessageNumber(def)) ]] <- def
    } else {
      local_message_number = localMessageNumber_header(record_header)
      definition <- msgDefs[[ as.character(local_message_number) ]]
      
      
      if(nzchar(Sys.getenv("DEBUG_FITFILER"))) {
          message("Definition:");
          message(str(definition))
      }
      
      ## is this a developer data definition message?
      if(globalMessageNumber(definition) == 207) {
          devMessages <- .readMessage_dev_data_id(con = con, header = record_header, 
                                                  definition = definition, devMessages = devMessages)
      } else if(globalMessageNumber(definition) == 206) {
          devMessages <- .readMessage_dev_data_field_definition(con = con, 
                                                               header = record_header, 
                                                               definition = definition,
                                                               devMessages = devMessages)
      } else {
          if(!hasDeveloperData(definition)) {
            messages[[ msg_count ]] <- .readMessage_data(con = con, 
                                                       header = record_header, 
                                                       definition = definition)
          } else {
              messages[[ msg_count ]] <- .readMessage_devdata(con = con, 
                                                             header = record_header,
                                                             definition = definition,
                                                             devMessages)
          }

        msg_count <- msg_count + 1
        
      }
    }
    
    prev_header <- record_header
    
  }
  
  ## trim any unused spaces we assigned when 'messages' was preallocated
  messages <- messages[seq_len(msg_count-1)]
  
  fit <- new("FitFile", header = file_header, messages = messages, developer_msg_defs = devMessages)
  return(fit)
}

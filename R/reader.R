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
      definition <- .matchDefinition(msgDefs, local_message_number = record_header@local_message_number)
      messages[[ msg_count ]] <- fitFileR:::.readMessage_data(con = con, 
                                                              header = record_header, 
                                                              definition = definition)
      msg_count <- msg_count + 1
    }
    
  }
  
  close(con)
  
  fit <- new("RawFitFile", header = file_header, messages = messages)
  return(fit)
}

.readFile_orig <- function(fileName) {
    
    con <- file(fileName, "rb")
    on.exit(close(con))
    file_header <- .readFileHeader(con)
    
    if(file_header$data_type != ".FIT") {
      stop("This does not look like a FIT file")
    }
    
    message_defs <- list()
    defs_idx <- 1
    
    plmt <- "-1"
    prev_lmt <- "0"
    defs_count <- list()
    pseudoMessageTab <- NULL
    
    scaffold <- list()
    
    count <- 1
     
    while(seek(con, where = NA) < (file_header$data_size + 14)) {
        
      #message(count)
      count <- count+1
      warnings()
      
        record_header <- .readRecordHeader(con)
        lmt <- as.character(record_header$local_message_type)

        if(record_header$message_type == "definition") {
            
          #message("Def: ", lmt)
          
            if(lmt %in% pseudoMessageTab[,2]) {
                plmt <- as.character(as.integer(plmt) + 1)
            } else {
                plmt <- lmt
            }
            pseudoMessageTab <- rbind(pseudoMessageTab, c(lmt, plmt))
            prev_lmt <- lmt
            
            ## read the message definition just to get through the bytes
            message_res <- .readMessage.definition(con, devFields = record_header$developer_data)
            message_defs[[ plmt ]] <- message_res$message
            defs_idx <- defs_idx + 1
            
            defs_count[[ plmt ]] <- 1
            
        } else if(record_header$message_type == "data") {
          
         #message("Data: ", lmt)
            
            if(record_header$type == "compressed_timestamp") {
             # message("Compressed")
              defIdx <- pseudoMessageTab[ max(which(pseudoMessageTab[,1] == lmt)), 2]
              message <- .readMessage.data(con, message_defs[[ defIdx ]], compressed_timestamp = TRUE)
              scaffold[[ defIdx ]] <- rbind(scaffold[[ defIdx ]], 
                                            message)
            } else {
          
              defIdx <- pseudoMessageTab[ max(which(pseudoMessageTab[,1] == lmt)), 2]
              message <- .readMessage.data(con, message_defs[[ defIdx ]], compressed_timestamp = FALSE)
              scaffold[[ defIdx ]] <- dplyr::bind_rows(scaffold[[ defIdx ]], 
                                            message) 
            }
          
            if( !is.null(message_defs[[ defIdx ]]$n_dev_fields) ) {
                skip <- sum(message_defs[[ defIdx ]]$dev_field_definition$size)
                #message("Skipping dev data: ", skip, " bytes")
                readBin(con, what = "integer", n = skip, size = 1)
            }
          
        } else {
            stop("unknown message type")
        }
    }
    
    if(length(message_defs) != length(scaffold)) {
        stop("Unequal lengths")
    } 
    
    scaffold <- lapply(scaffold, as_tibble)
    
    return(list(scaffold, message_defs, file_header))
}



#' @export
readFitFile <- function(fileName, dropUnknown = TRUE) {
  
  data("data_type_lookup", package = "fitFileR", envir = parent.frame())
  
  ## scan the file to find the number of each message type
  scan_result <- .scanFile(fileName)
  ## create a scaffold to fit the data in
  scaffold <- .buildMessageStructure(scan_result[[1]], scan_result[[2]])
  all_records <- .readFileWithScaffold(fileName, scaffold = scaffold, message_defs = scan_result[[1]])
  #all_records <- .readFileWithScaffold2(fileName, scaffold = scaffold, message_defs = scan_result[[1]])
  all_records <- .renameMessages(all_records, scan_result[[1]])
  
  for(i in names(all_records)) {
    all_records <- .processMessageType(all_records, name = i, drop = dropUnknown)
  }
  
  return(all_records)
}

## primary reading function, which extracts data from file and places
## it into the appropriate place in an already existing scaffold
.readFileWithScaffold <- function(fileName, scaffold, message_defs) {
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  file_header <- .readFileHeader(con)
  
  plmt <- "-1";
  prev_lmt <- "0"
  defs_count <- list()
  pseudoMessageTab <- NULL
  
  while(seek(con, where = NA) < (file_header$data_size + 14)) {
    
    record_header <- .readRecordHeader(con)
    lmt <- as.character(record_header$local_message_type)
    if(record_header$message_type == "definition") {
      
      if(lmt == prev_lmt) {
        plmt <- as.character(as.integer(plmt) + 1)
      } else {
        plmt <- lmt
      }
      pseudoMessageTab <- rbind(pseudoMessageTab, c(lmt, plmt))
      prev_lmt <- lmt
      
      ## read the message definition just to get through the bytes
      message <- .readMessage.definition(con, devFields = record_header$developer_data)
      
      defs_count[[ plmt ]] <- 1
      
    } else if(record_header$message_type == "data") {
      
      defIdx <- pseudoMessageTab[ max(which(pseudoMessageTab[,1] == lmt)), 2]
      message <- .readMessage.data(con, message_defs[[ defIdx ]])
      currentRow <- defs_count[[ defIdx ]]
      scaffold[[ defIdx ]][ currentRow , ] <- message$message
      defs_count[[ defIdx ]] <- defs_count[[ defIdx ]] + 1
      
    } else {
      stop("unknown message type")
    }
  }
  
  return(scaffold)
}


.readFileWithScaffold2 <- function(fileName, scaffold, message_defs) {
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  file_header <- .readFileHeader(con)
  
  plmt <- "-1";
  prev_lmt <- "0"
  defs_count <- list()
  pseudoMessageTab <- NULL
  
  while(seek(con, where = NA) < (file_header$data_size + 14)) {
    
    record_header <- .readRecordHeader(con)
    lmt <- as.character(record_header$local_message_type)
    if(record_header$message_type == "definition") {
      
      if(lmt == prev_lmt) {
        plmt <- as.character(as.integer(plmt) + 1)
      } else {
        plmt <- lmt
      }
      pseudoMessageTab <- rbind(pseudoMessageTab, c(lmt, plmt))
      prev_lmt <- lmt
      
      ## read the message definition just to get through the bytes
      message <- .readMessage.definition(con, devFields = record_header$developer_data)
      
      defs_count[[ plmt ]] <- 1
      
    } else if(record_header$message_type == "data") {
      
      defIdx <- pseudoMessageTab[ max(which(pseudoMessageTab[,1] == lmt)), 2]
      currentRow <- defs_count[[ defIdx ]]
      scaffold <- .readMessage.data2(con, message_defs[[ defIdx ]],
                                     scaffold, defIdx, currentRow)
      defs_count[[ defIdx ]] <- defs_count[[ defIdx ]] + 1
      
    } else {
      stop("unknown message type")
    }
  }
  
  return(scaffold)
}


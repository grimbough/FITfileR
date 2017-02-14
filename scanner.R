#source("~/projects/fitR/reader.R")

scanMessage.data <- function(con, definition) {
  
  fieldTypes <- definition$field_definition$base_type
  
  #bytesRead <- 0
  
  message <- list()
  for(i in 1:length(fieldTypes)) {
    readInfo <- data_type_lookup[[ fieldTypes[i] ]]
    if(readInfo[1] != "character") {
      #bytesRead <- bytesRead + (as.integer(readInfo[[3]]) * as.integer(readInfo[[4]]))
      bytesRead <- as.integer(readInfo[[3]]) * as.integer(readInfo[[4]])
      seek(con, where = bytesRead, origin = "current")
    } else {
      message <- readBin(con, what = "character", signed = readInfo[[2]],
                              size = readInfo[[3]], n = 1, 
                              endian = definition$architecture)
      bytesRead <- nchar(message)
    }
     
  }
  #seek(con, where = bytesRead, origin = "current")
  return(NULL)
}


scanFile <- function(fileName) {
  
  con <- file(fileName, "rb")
  on.exit(close(con))
  file_header <- readHeader(con)
  
  plmt <- "-1";
  prev_lmt <- "0"
  defs <- list()
  defs_count <- list()
  
  ## read some records
  for(j in 1:2000) {
    #while(bytesRead < file_header$data_size) {
    
    record_header <- readRecordHeader(con)
    lmt <- as.character(record_header$local_message_type)
    if(record_header$message_type == "definition") {
      cat(lmt, "\t")
      if(lmt == prev_lmt) {
        plmt <- as.character(as.integer(plmt) + 1)
        defs[[ as.character(plmt) ]] <- defs[[ as.character(prev_lmt) ]]
      } else {
        plmt <- lmt
      }
      prev_lmt <- lmt
      
      message <- readMessage.definition(con, devFields = record_header$developer_data)
      defs[[ as.character(lmt) ]] <- message$message
      defs_count[[ plmt ]] <- 0
      
      cat(plmt, "\t", defs[[ as.character(lmt) ]]$global_message_num, "\n")
      
    } else if(record_header$message_type == "data") {
      
      message <- scanMessage.data(con, defs[[ as.character(lmt) ]])
      defs_count[[ plmt ]] <- defs_count[[ plmt ]] + 1
      
    } else {
      stop("unknown message type")
    }
    #bytesRead <- bytesRead + message$bytesRead + 1
  }
  
  return(list(defs, defs_count))
}
